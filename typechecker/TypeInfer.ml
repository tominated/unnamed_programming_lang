open Base
open Ast.Syntax

open Result.Let_syntax

module StringMap = Map.M (String)
module StringSet = Set.M (String)

module TVarProvider = struct
  type t = unit -> Type.t

  let create (): t =
    let last_id = ref 0 in
    fun () ->
      let id = !last_id in
      last_id := id + 1;
      TypeVar (Printf.sprintf "t%d" id) |> locate
end

type location = Lexing.position * Lexing.position

type err =
  | VariableNotFound of { id: string; location: location }
  | InfiniteType
  | UnequalLengths
  | TypeMismatch of Type.t * Type.t
  | Unimplemented of string

let err_to_string e =
  match e with
  | VariableNotFound { id; _ } -> Printf.sprintf "Variable '%s' not found" id
  | InfiniteType -> "Infinite Type"
  | UnequalLengths -> "Unequal Lengths"
  | TypeMismatch (a, b) ->
      Printf.sprintf "Cannot unify '%s' with '%s'"
        (Type.to_string a)
        (Type.to_string b)
  | Unimplemented s -> Printf.sprintf "Unimplemented: %s" s

let locate (p: 'a) : 'a located = { item = p; location = (Lexing.dummy_pos, Lexing.dummy_pos) }

let generalise (env: TypeEnv.t) (t: Type.t) =
  let ftv_e = TypeEnv.free_type_vars env in
  let ftv_t = Type.free_type_vars t in
  let vars = Set.diff ftv_t ftv_e |> Set.to_list in
  Forall (vars, t)

(* TODO: This is probably gonna conflict with type vars made in infer *)
let instantiate (scheme: Scheme.t) (new_tvar: TVarProvider.t) =
  match scheme with
  | Forall (vars, t) ->
    let subs =
      List.fold
        ~init:TypeSubst.null
        ~f:(fun acc v -> Map.set acc ~key:v ~data:(new_tvar ()))
        vars
    in
    TypeSubst.type_apply subs t

let%test_module "instantiate" = (module struct
  let%expect_test "creates a new type" =
    let scheme = Forall ([], (TypeVar "a" |> locate)) in
    instantiate scheme (TVarProvider.create ()) |> Type.to_string |> Stdio.print_endline;
    [%expect {| a |}]

  let%expect_test "creates a type with vars" =
    let scheme = Forall (["a"; "b"; "c"], (TypeVar "a" |> locate)) in
    instantiate scheme (TVarProvider.create ()) |> Type.to_string |> Stdio.print_endline;
    [%expect {| t0 |}]

  let%expect_test "creates an arrow type with vars" =
    let arrow = TypeArrow (TypeVar "a" |> locate, TypeArrow (TypeVar "b" |> locate, TypeIdent "Number" |> locate) |> locate) |> locate in
    let scheme = Forall (["a"; "b"], arrow) in
    instantiate scheme (TVarProvider.create ()) |> Type.to_string |> Stdio.print_endline;
    [%expect {| t0 -> t1 -> Number |}]
end)


(** Given a type variable's name, and another type, get the substitutions *)
let var_bind name t =
  match (name, t.item) with
  (* If name is the same as a TypeVar then we don't know any substitutions *)
  | name, TypeVar m when String.equal name m ->
      Ok TypeSubst.null

  (* If name is found in the free type variables of t, then it fails the occurs check *)
  | name, _ when Set.mem (Type.free_type_vars t) name ->
      Error InfiniteType

  (* Otherwise substitute name with the type *)
  | _ -> Ok (TypeSubst.singleton name t)

let%test_module "var_bind" = (module struct
  let%expect_test "name and type var are equal" =
    var_bind "x" (locate (TypeVar "x"))
    |> Result.map ~f:(Map.length)
    |> Result.iter ~f:(Stdio.printf "%i");
    [%expect {| 0 |}]

  let%expect_test "free type variables return error" =
    let t = TypeArrow (locate (TypeVar "x"), locate (TypeVar "y")) |> locate in
    var_bind "x" t
    |> Result.iter_error ~f:(fun _ -> Stdio.printf "Error");
    [%expect {| Error |}]

  let%expect_test "x can be substitued" =
    var_bind "x" (locate (TypeIdent "Int"))
    |> Result.ok
    |> Option.bind ~f:(fun subs -> Map.find subs "x")
    |> Option.map ~f:Type.to_string
    |> Option.iter ~f:(Stdio.print_endline);
    [%expect {| Int |}]
end)

(** Given two types, determine if they can unify and return any possible
    type variable substitutions *)
let rec unify (t1: Type.t) (t2: Type.t) =
  match (t1.item, t2.item) with
  | TypeArrow (arg1, ret1), TypeArrow (arg2, ret2) ->
      unify_lists [arg1; ret1] [arg2; ret2]

  | _, TypeVar n -> var_bind n t1
  | TypeVar n, _ -> var_bind n t2

  | TypeIdent x, TypeIdent y when String.equal x y ->
      Ok TypeSubst.null

  | TypeConstructor (x, x_args), TypeConstructor (y, y_args)
    when String.equal x y ->
      unify_lists x_args y_args

  | TypeTuple xs, TypeTuple ys -> unify_lists xs ys

  | _ -> Error (TypeMismatch (t1, t2))

(** Given two lists of types, attempt to unify each element pairwise and
    aggregate the substitution mapping *)
and unify_lists xs ys =
  let folder acc x y =
    let%bind s1 = acc in
    let%map s2 = unify (TypeSubst.type_apply s1 x) (TypeSubst.type_apply s1 y) in
    TypeSubst.compose s2 s1
  in
  match List.fold2 ~f:folder ~init:(Ok TypeSubst.null) xs ys with
  | Ok x -> x
  | _ -> Error UnequalLengths

let%test_module "unify" = (module struct
  let%expect_test "Can unify ident with var" =
    let t1 = locate (TypeIdent "Test") in
    let t2 = locate (TypeVar "x") in
    unify t1 t2
    |> Result.ok
    |> Option.bind ~f:(fun subs -> Map.find subs "x")
    |> Option.map ~f:Type.to_string
    |> Option.iter ~f:(Stdio.print_endline);
    [%expect {| Test |}]

  let%expect_test "Can unify two arrows that are the same" =
    let t1 = locate (TypeArrow (locate (TypeIdent "Int"), locate (TypeIdent "String"))) in
    let t2 = locate (TypeArrow (locate (TypeIdent "Int"), locate (TypeIdent "String"))) in
    unify t1 t2
    |> Result.map ~f:Map.length
    |> Result.iter ~f:(Stdio.printf "%d");
    [%expect {| 0 |}]

  let%expect_test "Can unify two arrows complex arrows" =
  (* a -> String -> Bool, Int -> b -> Bool *)
    let t1 = locate (TypeArrow (locate (TypeVar "a"), locate (TypeArrow (locate (TypeIdent "String"), locate (TypeIdent "Bool"))))) in
    let t2 = locate (TypeArrow (locate (TypeIdent "Int"), locate (TypeArrow (locate (TypeVar "b"), locate (TypeIdent "Bool"))))) in
    unify t1 t2
    |> Result.iter ~f:(fun s -> TypeSubst.to_string s |> Stdio.print_endline);
    [%expect {| a = Int, b = String |}]

  let%expect_test "Does not unify two arrows that are different" =
    let t1 = locate (TypeArrow (locate (TypeIdent "Int"), locate (TypeIdent "String"))) in
    let t2 = locate (TypeArrow (locate (TypeIdent "Bool"), locate (TypeIdent "String"))) in
    unify t1 t2
    |> Result.map_error ~f:err_to_string
    |> Result.iter_error ~f:Stdio.print_endline;
    [%expect {| Cannot unify 'Int' with 'Bool' |}]

  let%expect_test "Can unify a generic and specialised arrow" =
    let t1 = locate (TypeArrow (locate (TypeVar "x"), locate (TypeIdent "String"))) in
    let t2 = locate (TypeArrow (locate (TypeIdent "Test"), locate (TypeIdent "String"))) in
    unify t1 t2
    |> Result.ok
    |> Option.bind ~f:(fun subs -> Map.find subs "x")
    |> Option.map ~f:Type.to_string
    |> Option.iter ~f:(Stdio.print_endline);
    [%expect {| Test |}]

  let%expect_test "Can unify a generic and specialised constructor" =
    let args1 = [locate (TypeVar "a"); locate (TypeIdent "Bool"); locate (TypeVar "c")] in
    let args2 = [locate (TypeIdent "Int"); locate (TypeVar "b"); locate (TypeIdent "String")] in
    let t1 = locate (TypeConstructor ("Test", args1)) in
    let t2 = locate (TypeConstructor ("Test", args2)) in
    unify t1 t2
    |> Result.iter ~f:(fun s -> TypeSubst.to_string s |> Stdio.print_endline);
    [%expect {| a = Int, b = Bool, c = String |}]

  let%expect_test "Can unify a generic and specialised tuple" =
    let xs = [locate (TypeVar "a"); locate (TypeIdent "Bool"); locate (TypeVar "c")] in
    let ys = [locate (TypeIdent "Int"); locate (TypeVar "b"); locate (TypeIdent "String")] in
    let t1 = locate (TypeTuple xs) in
    let t2 = locate (TypeTuple ys) in
    unify t1 t2
    |> Result.iter ~f:(fun s -> TypeSubst.to_string s |> Stdio.print_endline);
    [%expect {| a = Int, b = Bool, c = String |}]
end)

let rec infer (env: TypeEnv.t) (expr: expression) (new_tvar: TVarProvider.t): ((TypeSubst.t * Type.t), err) Result.t =
  match expr.item with
  | ExprUnit ->
      Ok (TypeSubst.null, locate TypeUnit)
  | ExprConstant const ->
    Ok (
      TypeSubst.null,
      match const with
      | ConstNumber _ -> locate (TypeIdent "Number")
      | ConstString _ -> locate (TypeIdent "String")
     )
  | ExprIdent id -> (
      match (Map.find env id) with
      | Some t -> Ok (TypeSubst.null, instantiate t new_tvar)
      | None -> Error (VariableNotFound { id; location = expr.location })
  )
  | ExprFn (param_id, body_expr) -> (
      let tvar = new_tvar () in
      let fn_env = Map.set env ~key:param_id ~data:(Forall ([], tvar)) in
      let%bind (subs, return_type) = infer fn_env body_expr new_tvar in
      Ok (subs, TypeArrow (TypeSubst.type_apply subs tvar, return_type) |> locate)
  )
  | ExprValBinding (pattern, value_expr, body_expr) -> (
    match pattern.item with
    | PatternVar var_name -> (
      let%bind (value_subs, value_type) = infer env value_expr new_tvar in
      let env_ = TypeSubst.env_apply value_subs env in
      let value_type_ = generalise env_ value_type in
      let%bind (body_subs, body_type) = infer (TypeEnv.extend env_ var_name value_type_) body_expr new_tvar in
      Ok (TypeSubst.compose value_subs body_subs, body_type)
    )
    | _ -> Error (Unimplemented "infer val binding for pattern")
  )
  | ExprApply (fn_expr, arg_expr) -> (
    let tvar = new_tvar () in
    let%bind (fn_subs, fn_type) = infer env fn_expr new_tvar in
    let%bind (body_subs, body_type) = infer (TypeSubst.env_apply fn_subs env) arg_expr new_tvar in
    let%bind rt_subs = unify (TypeSubst.type_apply body_subs fn_type) (TypeArrow (body_type, tvar) |> locate) in
    Ok (TypeSubst.compose rt_subs (TypeSubst.compose body_subs fn_subs), TypeSubst.type_apply rt_subs tvar)
  )
  | ExprInfix (lhs, op, rhs) -> (
    let%bind (lhs_subs, lhs_t) = infer env lhs new_tvar in
    let%bind (rhs_subs, rhs_t) = infer env rhs new_tvar in
    let%bind (_, op_t) = infer env (ExprIdent op |> locate) new_tvar in
    let tvar = new_tvar () in
    let%bind rt_subs = unify (TypeArrow (lhs_t, (TypeArrow (rhs_t, tvar) |> locate)) |> locate) op_t in
    Ok (TypeSubst.compose lhs_subs (TypeSubst.compose rhs_subs rt_subs), TypeSubst.type_apply rt_subs tvar)
  )
  | _ -> Error (Unimplemented "infer")


let infer_type (env: TypeEnv.t) (expr: expression): (Type.t, err) Result.t =
  infer env expr (TVarProvider.create ()) |> Result.map ~f:(fun (subs, t) -> TypeSubst.type_apply subs t)

let%test_module "infer_type" = (module struct
  let%expect_test "first try" =
    let const = ExprConstant (ConstNumber 3.) |> locate in
    let fn = ExprFn ("a", locate ExprUnit) |> locate in
    let app = ExprApply (fn, const) |> locate in
    infer_type (TypeEnv.empty) app
    |> Result.iter ~f:(fun t -> Type.to_string t |> Stdio.print_endline);
    [%expect {| () |}]

  let%expect_test "operators!" =
    let num_t = TypeIdent "Number" |> locate in
    let plus_t = TypeArrow (num_t, TypeArrow (num_t, num_t) |> locate) |> locate in
    let env = TypeEnv.extend TypeEnv.empty "+" (Forall ([], plus_t)) in
    let const_a = ExprConstant (ConstNumber 3.) |> locate in
    let const_b = ExprConstant (ConstNumber 7.) |> locate in
    let expr = ExprInfix (const_a, "+", const_b) |> locate in
    infer_type env expr
    |> Result.iter ~f:(fun t -> Type.to_string t |> Stdio.print_endline);
    [%expect {| Number |}]
end)