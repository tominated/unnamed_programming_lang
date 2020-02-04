open Base
open Ast.Syntax

module Error = struct
  type t =
    | UnboundVariable of string
    | InfiniteType of string * Type.t
    | Unimplemented of string
end

let locate (p: 'a) : 'a located = { item = p; location = (Lexing.dummy_pos, Lexing.dummy_pos) }

module Infer = struct
  module R = struct
    type t = TypeEnv.t
  end

  module W = struct
    type t = (Type.t * Type.t) List.t
    let empty = []
    let append a b = a @ b
  end

  module S = struct
    type t = int
  end

  module InferResult = struct
    module T = struct
      type 'a t = ('a, Error.t) Result.t
      let bind x ~f = Result.bind ~f x
      let return x = Ok x
      let map = `Custom Result.map
    end

    type 'a t = 'a T.t

    include Monad.Make(T)
  end

  module InferRWST = RWST.Make (R) (W) (S) (InferResult)
  include InferRWST

  (** [unify t1 t2] Add a type constraint to the monad context *)
  let unify (t1: Type.t) (t2: Type.t) = tell [(t1, t2)]

  (** [in_env] Get a new monad with a variable binding in its env *)
  let in_env id scheme m =
    local (fun e -> TypeEnv.extend e id scheme) m

  (** [fresh] Generate a new type variable in the monad context *)
  let fresh =
    let open Let_syntax in
    let%bind x = get in
    let%bind _ = put (x + 1) in
    return (TypeVar (Printf.sprintf "t%d" x) |> locate)

  (** [except e] Lift an error in to the monad context *)
  let except (e: Error.t) = lift (Error e)

  (** [instantiate scheme] Turn a polytype in to a monotype by generating new
      type variables for each polymorphic variable *)
  let instantiate (Forall (vars, t)) =
    let open Let_syntax in
    let%bind new_vars = List.map ~f:(fun _ -> fresh) vars |> all in
    let subst = TypeSubst.of_alist_exn (List.zip_exn vars new_vars) in
    return (TypeSubst.type_apply subst t)
  
  (** [generalise env t] Turn a monotype to a polytype by promoting free
      variables not in the environment to the scheme *)
  let generalise (env: TypeEnv.t) (t: Type.t) : Scheme.t =
    let ftv_e = TypeEnv.free_type_vars env in
    let ftv_t = Type.free_type_vars t in
    let vars = Set.diff ftv_t ftv_e |> Set.to_list in
    Forall (vars, t)

  (** [lookup id] Look up a variable in the environment, and instantiate it *)
  let lookup id =
    let open Let_syntax in
    let%bind env = ask in
    match TypeEnv.lookup env id with
    | None -> except (UnboundVariable id)
    | Some scheme -> instantiate scheme

  (** [infer e] Infer a type for an expression within monad context *)
  let rec infer (expr: expression) : Type.t t =
    let open Let_syntax in
    match expr.item with
    | ExprUnit -> return (TypeUnit |> locate)

    | ExprIdent id -> lookup id

    | ExprFn (arg, body) ->
        let%bind tv = fresh in
        let%bind t = in_env arg (Scheme.from_type tv) (infer body) in
        return (TypeArrow (tv, t) |> locate)

    | ExprApply (fn, arg) ->
        let%bind fn_type = infer fn in
        let%bind arg_type = infer arg in
        let%bind rt_type = fresh in
        let%bind _ = unify fn_type (TypeArrow (arg_type, rt_type) |> locate) in
        return rt_type

    | ExprValBinding (pattern, value, body) -> begin
      match pattern.item with
      | PatternVar id -> begin
        let%bind env = ask in
        let%bind value_type = infer value in
        let scheme = generalise env value_type in
        let%bind body_type = in_env id scheme (infer body) in
        return body_type
      end
      | _ -> except (Unimplemented "other patterns")
    end

    | ExprRecordEmpty -> return (TypeRecord (locate TypeRowEmpty) |> locate)

    | ExprRecordExtend (label, value, rest) ->
        let%bind field_t = fresh in
        let%bind rest_t = fresh in
        let record_rest_t = TypeRecord rest_t |> locate in
        let%bind value_t = infer value in
        let%bind rest_t = infer rest in
        let%bind _ = unify field_t value_t in
        let%bind _ = unify record_rest_t rest_t in
        TypeRecord (TypeRowExtend (label, field_t, rest_t) |> locate) |> locate
        |> return

    | _ -> except (Unimplemented "infer")

  (** [run_infer env expr] Infer a type and list of constraints for an expression *)
  let run_infer env expr : ((Type.t * W.t), Error.t) Result.t =
    let open Result.Let_syntax in
    let%bind (t, _, constraints) = runRWST (infer expr) env 0 in
    return (t, constraints)
end

module Solve = struct
  (** Given a type variable's name, and another type, get the substitutions *)
  let var_bind name t : (TypeSubst.t, Error.t) Result.t =
    match (name, t.item) with
    (* If name is the same as a TypeVar then we don't know any substitutions *)
    | name, TypeVar m when String.equal name m ->
        Ok TypeSubst.null

    (* If name is found in the free type variables of t, then it fails the occurs check *)
    | name, _ when Set.mem (Type.free_type_vars t) name ->
        Error (InfiniteType (name, t))

    (* Otherwise substitute name with the type *)
    | _ -> Ok (TypeSubst.singleton name t)

  (** Attempt to find a substitution that unifies 2 types *)
  let rec unify (t1: Type.t) (t2: Type.t) : (TypeSubst.t, Error.t) Result.t =
    match (t1.item, t1.item) with
    | (TypeVar v, _) -> var_bind v t2
    | (_, TypeVar v) -> var_bind v t1
    | _ -> Error (Unimplemented "unifies")

  (** Attempt to find a substitution for a list of type pairs *)
  and unifyMany (ts: (Type.t * Type.t) List.t) : (TypeSubst.t, Error.t) Result.t =
    let open Result.Let_syntax in
    match ts with
    | [] -> return TypeSubst.null
    | (t1, t2) :: rest ->
      let%bind subst1 = unify t1 t2 in
      let%bind subst2 = unifyMany rest in
      return (TypeSubst.compose subst2 subst1)

  (** Solve for a substitution given a list of type constraints *)
  let rec solve (subst: TypeSubst.t) (constraints: (Type.t * Type.t) List.t) : (TypeSubst.t, Error.t) Result.t =
    let open Result.Let_syntax in
    match constraints with
    | [] -> return subst
    | (t1, t2) :: rest ->
      let%bind subst1 = unify t1 t2 in
      let rest1 = List.map ~f:(fun (t1', t2') ->
        (TypeSubst.type_apply subst1 t1', TypeSubst.type_apply subst1 t2')
      ) rest in
      solve (TypeSubst.compose subst1 subst) rest1

  (** Solve for a type given a list of constraints *)
  let run_solve t constraints : (Type.t, Error.t) Result.t =
    let open Result.Let_syntax in
    let%bind subst = solve TypeSubst.null constraints in
    return (TypeSubst.type_apply subst t)
end

(** Infer a type from an expression *)
let run env expr : (Type.t, Error.t) Result.t =
  let open Result.Let_syntax in
  let%bind (t, constraints) = Infer.run_infer env expr in
  Solve.run_solve t constraints
