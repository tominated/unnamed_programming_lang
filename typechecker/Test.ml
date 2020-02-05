open Base
open Ast.Syntax

module Error = struct
  type t =
    | UnboundVariable of string
    | InfiniteType of string * Type.t
    | UnequalLengths of (Type.t List.t) * (Type.t List.t)
    | TypeMismatch of Type.t * Type.t
    | LabelNotFound of string
    | RecursiveRowType
    | UnexpectedType of Type.t
    | Unimplemented of string

  let to_string = function
  | UnboundVariable id -> Printf.sprintf "Variable '%s' not found" id
  | InfiniteType _ -> "Infinite Type"
  | UnequalLengths _ -> "Unequal Lengths"
  | TypeMismatch (a, b) ->
      Printf.sprintf "Cannot unify '%s' with '%s'"
        (Type.to_string a)
        (Type.to_string b)
  | LabelNotFound l -> Printf.sprintf "Label '%s' not found" l
  | RecursiveRowType -> "Recursive row type"
  | UnexpectedType t -> Printf.sprintf "Unexpected type '%s'" (Type.to_string t)
  | Unimplemented s -> Printf.sprintf "Unimplemented: %s" s
end

let locate (p: 'a) : 'a located = { item = p; location = (Lexing.dummy_pos, Lexing.dummy_pos) }

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

module TypeVarState = struct
  type t = int

  let letters = [|
    'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
    'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
  |]
  let num_letters = Array.length letters
  let as_id n =
    let num_iters = n / num_letters in
    let char = Array.get letters (n % num_letters) in
    if num_iters > 0
    then Printf.sprintf "%c%i" char num_iters
    else Printf.sprintf "%c" char

  let as_type n =
    TypeVar (as_id n) |> locate
end

module Infer = struct
  module R = struct
    type t = TypeEnv.t
  end

  module W = struct
    type t = (Type.t * Type.t) List.t
    let empty = []
    let append a b = a @ b
  end

  module InferRWST = RWST.Make (R) (W) (TypeVarState) (InferResult)
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
    return (TypeVarState.as_type x)

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

    | ExprConstant const -> begin
        match const with
        | ConstNumber _ -> TypeIdent "Number" |> locate |> return
        | ConstString _ -> TypeIdent "String" |> locate |> return
      end

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

    | ExprInfix (lhs, op, rhs) ->
        let%bind rt_t = fresh in
        let%bind lhs_t = infer lhs in
        let%bind rhs_t = infer rhs in
        let%bind op_t = infer (ExprIdent op |> locate) in
        let fn_t = TypeArrow (lhs_t, (TypeArrow (rhs_t, rt_t) |> locate)) |> locate in
        let%bind _ = unify fn_t op_t in
        return rt_t

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

    | ExprRecordAccess (expr, label) ->
        let%bind field_t = fresh in
        let%bind rest_t = fresh in
        let record_t = TypeRecord (TypeRowExtend (label, field_t, rest_t) |> locate) |> locate in
        let%bind expr_t = infer expr in
        let%bind _ = unify record_t expr_t in
        return field_t

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

    | ExprTuple _ -> except (Unimplemented "infer tuple")
    | ExprAnnotated _ -> except (Unimplemented "infer annotated")
    | ExprTypeBinding _ -> except (Unimplemented "infer type binding")
    | ExprMatch _ -> except (Unimplemented "infer match")
    | ExprIfElse _ -> except (Unimplemented "infer ifelse")
    | ExprConstruct _ -> except (Unimplemented "infer construct")
    | ExprArray _ -> except (Unimplemented "infer array")
    | ExprSequence _ -> except (Unimplemented "infer sequence")

  (** [run_infer env expr] Infer a type and list of constraints for an expression *)
  let run_infer env expr : ((Type.t * TypeVarState.t * W.t), Error.t) Result.t =
    runRWST (infer expr) env 0
end

module Solve = struct
  module SolveStateT = StateT.Make (TypeVarState) (InferResult)
  include SolveStateT

  (** [except e] Lift an error in to the monad context *)
  let except (e: Error.t) = lift (Error e)

  (** [fresh] Generate a new type variable in the monad context *)
  let fresh =
    let open Let_syntax in
    let%bind x = get in
    let%bind _ = put (x + 1) in
    return (TypeVarState.as_type x)

  (** Given a type variable's name, and another type, get the substitutions *)
  let var_bind name t : TypeSubst.t t =
    match (name, t.item) with
    (* If name is the same as a TypeVar then we don't know any substitutions *)
    | name, TypeVar m when String.equal name m ->
        return TypeSubst.null

    (* If name is found in the free type variables of t, then it fails the occurs check *)
    | name, _ when Set.mem (Type.free_type_vars t) name ->
        except (InfiniteType (name, t))

    (* Otherwise substitute name with the type *)
    | _ -> return (TypeSubst.singleton name t)

  type rewrite = { field: Type.t; tail: Type.t; subst: TypeSubst.t }
  let rec rewrite_row (row2: Type.t) (label1: string) : rewrite t =
    let open Let_syntax in
    match row2.item with
    | TypeRowExtend (label, field, tail) when String.equal label1 label ->
        return { field; tail; subst = TypeSubst.null }
    | TypeRowExtend (label, field, tail) -> begin
        match tail.item with
        | TypeVar alpha ->
            let%bind beta = fresh in
            let%bind gamma = fresh in
            return {
              field = gamma;
              tail = TypeRowExtend (label, field, beta) |> locate;
              subst = TypeSubst.singleton alpha (TypeRowExtend (label1, gamma, beta) |> locate)
            }
        | _ ->
            let%bind {field = field'; tail = tail'; subst} = rewrite_row tail label1 in
            return {
              field = field';
              tail = TypeRowExtend (label, field, tail') |> locate;
              subst
            }
      end
    | TypeRowEmpty -> except (LabelNotFound label1)
    | _ -> except (UnexpectedType row2)

  (** Attempt to find a substitution that unifies 2 types *)
  let rec unify (t1: Type.t) (t2: Type.t) : TypeSubst.t t =
    let open Let_syntax in
    match (t1.item, t2.item) with
    | TypeVar v, _ -> var_bind v t2
    | _, TypeVar v -> var_bind v t1

    | TypeArrow (arg1, rt1), TypeArrow (arg2, rt2) ->
        unify_many [arg1; rt1] [arg2; rt2]

    | TypeIdent a, TypeIdent b when String.equal a b ->
        return TypeSubst.null

    | TypeConstructor (a, a_args), TypeConstructor (b, b_args) ->
        unify_many (a :: a_args) (b :: b_args)

    | TypeRecord r1, TypeRecord r2 -> unify r1 r2
    | TypeRowEmpty, TypeRowEmpty -> return TypeSubst.null

    | TypeRowExtend (label1, field1, tail1), TypeRowExtend (_, _, _) -> begin
        let%bind { field = field2; tail = tail2; subst = subst1 } = rewrite_row t2 label1 in
        match tail2.item with
        | TypeRowExtend(_, _, { item = TypeVar tv; _ }) when TypeSubst.mem subst1 tv ->
            except RecursiveRowType
        | _ ->
          let%bind subst2 = unify (TypeSubst.type_apply subst1 field1) (TypeSubst.type_apply subst1 field2) in
          let subst3 = TypeSubst.compose subst2 subst1 in
          let%bind subst4 = unify (TypeSubst.type_apply subst3 tail1) (TypeSubst.type_apply subst3 tail2) in
          return (TypeSubst.compose subst4 subst3)
      end

    | TypeRowEmpty, TypeRowExtend _
    | TypeRowExtend _, TypeRowEmpty -> except (TypeMismatch (t1, t2))

    | _ -> except (Unimplemented "unifies")

  (** Attempt to find a substitution for a list of type pairs *)
  and unify_many (t1s: Type.t List.t) (t2s: Type.t List.t) : TypeSubst.t t =
    let open Let_syntax in
    match t1s, t2s with
    | [], [] -> return TypeSubst.null
    | t1 :: t1rest, t2 :: t2rest ->
        let%bind subst1 = unify t1 t2 in
        let%bind subst2 = unify_many t1rest t2rest in
        return (TypeSubst.compose subst2 subst1)
    | _ -> except (UnequalLengths (t1s, t2s))

  (** Solve for a substitution given a list of type constraints *)
  let rec solve (subst: TypeSubst.t) (constraints: (Type.t * Type.t) List.t) : TypeSubst.t t =
    let open Let_syntax in
    match constraints with
    | [] -> return subst
    | (t1, t2) :: rest ->
      let%bind subst1 = unify t1 t2 in
      let rest1 = List.map ~f:(fun (t1', t2') ->
        (TypeSubst.type_apply subst1 t1', TypeSubst.type_apply subst1 t2')
      ) rest in
      solve (TypeSubst.compose subst1 subst) rest1

  (** Solve for a type given a list of constraints *)
  let run_solve (s : TypeVarState.t) (t: Type.t) constraints : (Type.t, Error.t) Result.t =
    let open Result.Let_syntax in
    let%bind (subst, _) = runStateT (solve TypeSubst.null constraints) s in
    return (TypeSubst.type_apply subst t)
end

(** Infer a type from an expression *)
let run env expr : (Type.t, Error.t) Result.t =
  let open Result.Let_syntax in
  let%bind (t, s, constraints) = Infer.run_infer env expr in
  Solve.run_solve s t constraints
