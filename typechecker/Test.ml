open Base
open Stdio

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

  let as_type n = TypeVar (as_id n)
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
  let unify (t1: Type.t) (t2: Type.t) =
    Stdio.printf "constraint: (%s) == (%s)\n" (Type.to_string t1) (Type.to_string t2);
    tell [(t1, t2)]

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

  let%test_module "instantiate" = (module struct
    let run_test scheme =
      Result.iter
        ~f:(fun (t, _, _) -> Stdio.print_endline (Type.to_string t))
        (runRWST (instantiate scheme) TypeEnv.empty 0)
    
    let%expect_test "creates a new type" =
      let scheme = Forall ([], TypeVar "a") in
      run_test scheme;
      [%expect {| a |}]
    
    let%expect_test "creates a type with vars" =
      let scheme = Forall (["a"; "b"; "c"], TypeVar "a") in
      run_test scheme;
      [%expect {| a |}]
    
    let%expect_test "creates an arrow type with vars" =
      let arrow = TypeArrow (TypeVar "a", TypeArrow (TypeVar "b", TypeIdent "Number")) in
      let scheme = Forall (["a"; "b"], arrow) in
      run_test scheme;
      [%expect {| (a -> (b -> Number)) |}]
  end)

  (** [generalise env t] Turn a monotype to a polytype by promoting free
      variables not in the environment to the scheme *)
  let generalise (env: TypeEnv.t) (t: Type.t) : Scheme.t =
    let ftv_e = TypeEnv.free_type_vars env in
    let ftv_t = Type.free_type_vars t in
    let vars = Set.diff ftv_e ftv_t |> Set.to_list in
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
    | ExprUnit -> return TypeUnit

    | ExprConstant const ->
        Stdio.print_endline "infer const";
        return begin
          match const with
          | ConstNumber _ -> TypeIdent "Number"
          | ConstString _ -> TypeIdent "String"
        end

    | ExprIdent id ->
        Stdio.print_endline "infer ident";
        lookup id

    | ExprFn (arg, body) ->
        Stdio.print_endline "infer fn";
        let%bind tv = fresh in
        let%bind t = in_env arg (Scheme.from_type tv) (infer body) in
        return (TypeArrow (tv, t))

    | ExprApply (fn, arg) ->
        Stdio.print_endline "infer apply";
        let%bind fn_type = infer fn in
        let%bind arg_type = infer arg in
        let%bind rt_type = fresh in
        let%bind _ = unify fn_type (TypeArrow (arg_type, rt_type)) in
        return rt_type

    | ExprInfix (lhs, op, rhs) ->
        Stdio.print_endline "infer infix";
        let%bind rt_t = fresh in
        let%bind lhs_t = infer lhs in
        let%bind rhs_t = infer rhs in
        let%bind op_t = lookup op in
        let fn_t = TypeArrow (lhs_t, TypeArrow (rhs_t, rt_t)) in
        let%bind _ = unify fn_t op_t in
        return rt_t

    | ExprValBinding (pattern, value, body) -> begin
      Stdio.print_endline "infer let";
      match pattern.item with
      | PatternVar id -> begin
        let%bind env = ask in
        let%bind value_t = infer value in
        let scheme = generalise env value_t in
        in_env id scheme (infer body)
      end
      | _ -> except (Unimplemented "other patterns")
    end

    | ExprRecordAccess (expr, label) ->
        Stdio.print_endline "infer record access";
        let%bind field_t = fresh in
        let%bind rest_t = fresh in
        let record_t = TypeRecord (TypeRowExtend (label, field_t, rest_t)) in
        let%bind expr_t = infer expr in
        let%bind _ = unify record_t expr_t in
        return field_t

    | ExprRecordEmpty ->
        Stdio.print_endline "infer record empty";
        return (TypeRecord TypeRowEmpty)

    | ExprRecordExtend (label, value, rest) ->
        Stdio.print_endline "infer record extend";
        let%bind field_t = fresh in
        let%bind rest_t = fresh in
        let param1_t = field_t in
        let param2_t = TypeRecord rest_t in
        let rt_t = TypeRecord (TypeRowExtend (label, field_t, rest_t)) in
        let%bind value_t = infer value in
        let%bind _ = unify param1_t value_t in
        let%bind rest_t = infer rest in
        let%bind _ = unify param2_t rest_t in
        return rt_t

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
    Stdio.printf "var_bind %s to %s\n" name (Type.to_string t);
    match (name, t) with
    (* If name is the same as a TypeVar then we don't know any substitutions *)
    | name, TypeVar m when String.equal name m ->
        return TypeSubst.null

    (* If name is found in the free type variables of t, then it fails the occurs check *)
    | name, _ when Set.mem (Type.free_type_vars t) name ->
        except (InfiniteType (name, t))

    (* Otherwise substitute name with the type *)
    | _ -> return (TypeSubst.singleton name t)

  let%test_module "var_bind" = (module struct
    let%expect_test "does not substitute when name and type are equal" =
      Result.iter
        ~f:(fun (t, _) -> Stdio.print_endline (TypeSubst.to_string t))
        (runStateT (var_bind "x" (TypeVar "x")) 0);
      [%expect {| |}]
    
    let%expect_test "x can be substitued for an identifier" =
      Result.iter
        ~f:(fun (t, _) -> Stdio.print_endline (TypeSubst.to_string t))
        (runStateT (var_bind "x" (TypeIdent "Int")) 0);
      [%expect {| x = Int |}]
    
    let%expect_test "x can be substitued for another variable" =
      Result.iter
        ~f:(fun (t, _) -> Stdio.print_endline (TypeSubst.to_string t))
        (runStateT (var_bind "x" (TypeVar "y")) 0);
      [%expect {| x = y |}]
    
    let%expect_test "free type variables return error" =
      let t = TypeArrow (TypeVar "x",TypeVar "y") in
      Result.iter_error
        ~f:(fun e -> e |> Error.to_string |> Stdio.print_endline)
        (runStateT (var_bind "x" t) 0);
      [%expect {| Infinite Type |}]
  end)

  type rewrite = { field: Type.t; tail: Type.t; subst: TypeSubst.t }
  let rec rewrite_row (row2: Type.t) (label1: string) : rewrite t =
    Stdio.printf "rewrite row: '%s' in %s\n" label1 (Type.to_string row2);
    let open Let_syntax in
    match row2 with
    | TypeRecord t -> rewrite_row t label1
    | TypeRowExtend (label, field, tail) when String.equal label1 label ->
        return { field; tail; subst = TypeSubst.null }
    | TypeRowExtend (label, field, tail) -> begin
        match tail with
        | TypeVar alpha ->
            let%bind beta = fresh in
            let%bind gamma = fresh in
            return {
              field = gamma;
              tail = TypeRowExtend (label, field, beta);
              subst = TypeSubst.singleton alpha (TypeRowExtend (label1, gamma, beta))
            }
        | _ ->
            let%bind {field = field'; tail = tail'; subst} = rewrite_row tail label1 in
            return {
              field = field';
              tail = TypeRowExtend (label, field, tail');
              subst
            }
      end
    | TypeRowEmpty -> except (LabelNotFound label1)
    | _ -> except (UnexpectedType row2)

  (** Attempt to find a substitution that unifies 2 types *)
  let rec unify (t1: Type.t) (t2: Type.t) : TypeSubst.t t =
    Stdio.printf "unify (%s) == (%s)\n" (Type.to_string t1) (Type.to_string t2);
    let open Let_syntax in
    match (t1, t2) with
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
        match tail2 with
        | TypeRowExtend(_, _, TypeVar tv) when TypeSubst.mem subst1 tv ->
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
    Stdio.printf "subst: %s\n" (TypeSubst.to_string subst);
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
