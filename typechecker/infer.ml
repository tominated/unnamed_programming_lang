open Base
open Ast.Syntax

open Result.Let_syntax

module StringMap = Map.M (String)
module StringSet = Set.M (String)

module rec Substitution: sig
  type t = Type.t StringMap.t
  val null: t
  val singleton: string -> Type.t -> t
  val to_string: t -> string
  val compose: t -> t -> t
end = struct
  type t = Type.t StringMap.t

  let null : t = Map.empty (module String)
  let singleton s t : t = Map.singleton (module String) s t

  let to_string (s: t) =
    Map.to_alist s
    |> List.map ~f:(fun (v, t) -> Printf.sprintf "%s = %s" v (Type.to_string t))
    |> String.concat ~sep:", "


  (** Given two substitutions, merge them and try to apply any possible substitutions *)
  let compose (s1: t) (s2: t) =
    Map.merge
      ~f:(fun ~key:_ x -> match x with `Left v | `Right v | `Both (_, v) -> Some v)
      (Map.map ~f:(Type.apply s1) s2)
      s1

end

and Scheme: sig
  type t = Forall of string list * Type.t
  val apply: Substitution.t -> t -> t
  val free_type_vars: t -> StringSet.t
  val to_string: t -> string
end = struct
  type t = Forall of string list * Type.t

  let apply (subs: Substitution.t) (scheme: t) =
    match scheme with
    | Forall (vars, t) ->
      let subs_ = vars |> List.fold ~init:subs ~f:Map.remove in
      Forall (vars, Type.apply subs_ t)

  let free_type_vars (scheme: t): StringSet.t =
    match scheme with
    | Forall (vars, t) ->
      let ftv_s = Set.of_list (module String) vars in
      let ftv_t = Type.free_type_vars t in
      Set.diff ftv_s ftv_t

  let to_string (scheme: t): string =
    match scheme with
    | Forall (vars, t) ->
        Printf.sprintf "forall %s. %s"
          (String.concat ~sep:" " vars)
          (Type.to_string t)

end

and Env: sig
  type t = Scheme.t StringMap.t
  val empty: t
  val set: t -> string -> Scheme.t -> t
  val find: t -> string -> Scheme.t option
  val remove: t -> string -> t
  val apply: Substitution.t -> t -> t
  val free_type_vars: t -> StringSet.t
end = struct
  type t = Scheme.t StringMap.t

  let empty: t = Map.empty (module String)
  let set (m: t) (k: String.t) (v: Scheme.t): t = Map.set m ~key:k ~data:v
  let find (m: t) (k: String.t): Scheme.t Option.t = Map.find m k
  let remove (m: t) (k: String.t): t = Map.remove m k

  let apply (subs: Substitution.t) (env: t): t =
    env |> Map.map ~f:(Scheme.apply subs)

  let free_type_vars (env: t): StringSet.t =
    Map.data env
    |> List.fold ~f:(fun set scheme -> Set.union set (Scheme.free_type_vars scheme)) ~init:(Set.empty (module String))
end

and Type: sig
  type t = type_signature
  val apply: Substitution.t -> t -> t
  val free_type_vars: t -> StringSet.t
  val to_string: t -> string
end = struct
  type t = type_signature

  (** Apply substitutions to a type *)
  let rec apply (subst: Substitution.t) (t: t) =
    match t.item with
    | TypeVar v ->
      Map.find subst v |> Option.value ~default:t
    | TypeArrow (t1, t2) -> {
        item = TypeArrow (apply subst t1, apply subst t2);
        location = t.location
      }
    | TypeTuple ts -> {
        item = TypeTuple (List.map ~f:(apply subst) ts);
        location = t.location;
      }
    | TypeConstructor (n, ts) -> {
        item = TypeConstructor (n, List.map ~f:(apply subst) ts);
        location = t.location;
      }
    | _ -> t

  let rec free_type_vars (t: t): StringSet.t =
    match t.item with
    | TypeVar a -> Set.singleton (module String) a
    | TypeArrow (t1, t2) ->
        Set.union (free_type_vars t1) (free_type_vars t2)
    | TypeTuple ts ->
        ts |> List.map ~f:free_type_vars |> Set.union_list (module String)
    | _ -> Set.empty (module String)

  let to_string = type_signature_to_string

end

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

let generalise (env: Env.t) (t: Type.t) =
  let ftv_e = Env.free_type_vars env in
  let ftv_t = Type.free_type_vars t in
  let vars = Set.diff ftv_t ftv_e |> Set.to_list in
  Scheme.Forall (vars, t)

(* TODO: This is probably gonna conflict with type vars made in infer *)
let instantiate (scheme: Scheme.t) (new_tvar: TVarProvider.t) =
  match scheme with
  | Forall (vars, t) ->
    let subs =
      List.fold
        ~init:Substitution.null
        ~f:(fun acc v -> Map.set acc ~key:v ~data:(new_tvar ()))
        vars
    in
    Type.apply subs t

let%test_module "instantiate" = (module struct
  let%expect_test "creates a new type" =
    let scheme = Scheme.Forall ([], (TypeVar "a" |> locate)) in
    instantiate scheme (TVarProvider.create ()) |> Type.to_string |> Stdio.print_endline;
    [%expect {| a |}]

  let%expect_test "creates a type with vars" =
    let scheme = Scheme.Forall (["a"; "b"; "c"], (TypeVar "a" |> locate)) in
    instantiate scheme (TVarProvider.create ()) |> Type.to_string |> Stdio.print_endline;
    [%expect {| t0 |}]

  let%expect_test "creates an arrow type with vars" =
    let arrow = TypeArrow (TypeVar "a" |> locate, TypeArrow (TypeVar "b" |> locate, TypeIdent "Number" |> locate) |> locate) |> locate in
    let scheme = Scheme.Forall (["a"; "b"], arrow) in
    instantiate scheme (TVarProvider.create ()) |> Type.to_string |> Stdio.print_endline;
    [%expect {| t0 -> t1 -> Number |}]
end)


(** Given a type variable's name, and another type, get the substitutions *)
let var_bind name t =
  match (name, t.item) with
  (* If name is the same as a TypeVar then we don't know any substitutions *)
  | name, TypeVar m when String.equal name m ->
      Ok Substitution.null

  (* If name is found in the free type variables of t, then it fails the occurs check *)
  | name, _ when Set.mem (Type.free_type_vars t) name ->
      Error InfiniteType

  (* Otherwise substitute name with the type *)
  | _ -> Ok (Substitution.singleton name t)

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
      Ok Substitution.null

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
    let%map s2 = unify (Type.apply s1 x) (Type.apply s1 y) in
    Substitution.compose s2 s1
  in
  match List.fold2 ~f:folder ~init:(Ok Substitution.null) xs ys with
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
    |> Result.iter ~f:(fun s -> Substitution.to_string s |> Stdio.print_endline);
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
    |> Result.iter ~f:(fun s -> Substitution.to_string s |> Stdio.print_endline);
    [%expect {| a = Int, b = Bool, c = String |}]

  let%expect_test "Can unify a generic and specialised tuple" =
    let xs = [locate (TypeVar "a"); locate (TypeIdent "Bool"); locate (TypeVar "c")] in
    let ys = [locate (TypeIdent "Int"); locate (TypeVar "b"); locate (TypeIdent "String")] in
    let t1 = locate (TypeTuple xs) in
    let t2 = locate (TypeTuple ys) in
    unify t1 t2
    |> Result.iter ~f:(fun s -> Substitution.to_string s |> Stdio.print_endline);
    [%expect {| a = Int, b = Bool, c = String |}]
end)

let rec infer (env: Env.t) (expr: expression) (new_tvar: TVarProvider.t): ((Substitution.t * Type.t), err) Result.t =
  match expr.item with
  | ExprUnit ->
      Ok (Substitution.null, locate TypeUnit)
  | ExprConstant const ->
    Ok (
      Substitution.null,
      match const with
      | ConstNumber _ -> locate (TypeIdent "Number")
      | ConstString _ -> locate (TypeIdent "String")
     )
  | ExprIdent id -> (
      match (Map.find env id) with
      | Some t -> Ok (Substitution.null, instantiate t new_tvar)
      | None -> Error (VariableNotFound { id; location = expr.location })
  )
  | ExprFn (param_id, body_expr) -> (
      let tvar = new_tvar () in
      let fn_env = Map.set env ~key:param_id ~data:(Scheme.Forall ([], tvar)) in
      let%bind (subs, return_type) = infer fn_env body_expr new_tvar in
      Ok (subs, TypeArrow (Type.apply subs tvar, return_type) |> locate)
  )
  | ExprValBinding (pattern, value_expr, body_expr) -> (
    match pattern.item with
    | PatternVar var_name -> (
      let%bind (value_subs, value_type) = infer env value_expr new_tvar in
      let env_ = Env.apply value_subs env in
      let value_type_ = generalise env_ value_type in
      let%bind (body_subs, body_type) = infer (Env.set env_ var_name value_type_) body_expr new_tvar in
      Ok (Substitution.compose value_subs body_subs, body_type)
    )
    | _ -> Error (Unimplemented "infer val binding for pattern")
  )
  | ExprApply (fn_expr, arg_expr) -> (
    let tvar = new_tvar () in
    let%bind (fn_subs, fn_type) = infer env fn_expr new_tvar in
    let%bind (body_subs, body_type) = infer (Env.apply fn_subs env) arg_expr new_tvar in
    let%bind rt_subs = unify (Type.apply body_subs fn_type) (TypeArrow (body_type, tvar) |> locate) in
    Ok (Substitution.compose rt_subs (Substitution.compose body_subs fn_subs), Type.apply rt_subs tvar)
  )
  | ExprInfix (lhs, op, rhs) -> (
    let%bind (lhs_subs, lhs_t) = infer env lhs new_tvar in
    let%bind (rhs_subs, rhs_t) = infer env rhs new_tvar in
    let%bind (_, op_t) = infer env (ExprIdent op |> locate) new_tvar in
    let tvar = new_tvar () in
    let%bind rt_subs = unify (TypeArrow (lhs_t, (TypeArrow (rhs_t, tvar) |> locate)) |> locate) op_t in
    Ok (Substitution.compose lhs_subs (Substitution.compose rhs_subs rt_subs), Type.apply rt_subs tvar)
  )
  | _ -> Error (Unimplemented "infer")


let infer_type (env: Env.t) (expr: expression): (Type.t, err) Result.t =
  infer env expr (TVarProvider.create ()) |> Result.map ~f:(fun (subs, t) -> Type.apply subs t)

let%test_module "infer_type" = (module struct
  let%expect_test "first try" =
    let const = ExprConstant (ConstNumber 3.) |> locate in
    let fn = ExprFn ("a", locate ExprUnit) |> locate in
    let app = ExprApply (fn, const) |> locate in
    infer_type (Env.empty) app
    |> Result.iter ~f:(fun t -> Type.to_string t |> Stdio.print_endline);
    [%expect {| () |}]

  let%expect_test "operators!" =
    let num_t = TypeIdent "Number" |> locate in
    let plus_t = TypeArrow (num_t, TypeArrow (num_t, num_t) |> locate) |> locate in
    let env = Env.set Env.empty "+" (Scheme.Forall ([], plus_t)) in
    let const_a = ExprConstant (ConstNumber 3.) |> locate in
    let const_b = ExprConstant (ConstNumber 7.) |> locate in
    let expr = ExprInfix (const_a, "+", const_b) |> locate in
    infer_type env expr
    |> Result.iter ~f:(fun t -> Type.to_string t |> Stdio.print_endline);
    [%expect {| Number |}]
end)
