open Base

module StringMap = Map.M (String)
module StringSet = Set.M (String)

type ty =
  | TypeVar of string (* eg. a, b *)
  | TypeConstructor of string (* eg. Int, Boolean, List *)
  | TypeArrow of ty * ty (* eg. Int -> Boolean *)
  | TypeApplication of ty * ty (* eg. List Int *)

let rec ty_to_string (t: ty) =
  match t with
  | TypeVar v -> v
  | TypeConstructor id -> id
  | TypeArrow (lhs, rhs) -> Printf.sprintf "%s -> %s" (ty_to_string lhs) (ty_to_string rhs)
  | TypeApplication (lhs, rhs) -> Printf.sprintf "%s %s" (ty_to_string lhs) (ty_to_string rhs)

type kind =
  | KindType
  | KindArrow of kind * kind
  | KindVar of string

let rec kind_to_string (k: kind) = 
  match k with
  | KindType -> "Type"
  | KindArrow (lhs, rhs) -> Printf.sprintf "%s -> %s" (kind_to_string lhs) (kind_to_string rhs)
  | KindVar v -> v

(* Maybe: KindType -> KindType *)
type kind_env = kind StringMap.t
type substitution = kind StringMap.t

let empty_substitution = Map.empty (module String)

let rec apply_substitution (subs: substitution) (k: kind) =
  match k with
  | KindVar v -> Map.find subs v |> Option.value ~default:k
  | KindArrow (k1, k2) ->
      KindArrow (apply_substitution subs k1, apply_substitution subs k2)
  | KindType -> k

let apply_substitution_to_env (subs: substitution) (e: kind_env) =
  Map.map ~f:(apply_substitution subs) e

let substitution_compose (s1: substitution) (s2: substitution) =
    Map.merge
      ~f:(fun ~key:_ x -> match x with `Left v | `Right v | `Both (_, v) -> Some v)
      (Map.map ~f:(apply_substitution s1) s2)
      s1

let rec free_kind_vars (k: kind) =
  match k with
  | KindVar v -> Set.singleton (module String) v
  | KindType -> Set.empty (module String)
  | KindArrow (l, r) -> Set.union (free_kind_vars l) (free_kind_vars r)

module KindVarProvider = struct
  type t = unit -> kind

  let create (): t =
    let last_id = ref 0 in
    fun () ->
      let id = !last_id in
      last_id := id + 1;
      KindVar (Printf.sprintf "k%d" id)
end

let solve_kind (v: string) (k: kind) =
  match (v, k) with
  | v, KindVar v_ when String.equal v v_ -> empty_substitution
  | v, _ when Set.mem (free_kind_vars k) v ->
      failwith "infinite kind"
  | _ -> Map.singleton (module String) v k

let rec unify (k1: kind) (k2: kind) =
  match (k1, k2) with
  | KindVar v1, KindVar v2 when String.equal v1 v2 -> empty_substitution
  | KindVar v, k | k, KindVar v -> solve_kind v k
  | KindType, KindType -> empty_substitution
  | KindArrow (l1, r1), KindArrow(l2, r2) ->
      let s1 = unify l1 l2 in
      let s2 = unify (apply_substitution s1 r1) (apply_substitution s1 r2) in
      substitution_compose s2 s1
  | _ ->
    Stdio.print_endline (kind_to_string k1);
    Stdio.print_endline (kind_to_string k2);
    failwith "kind mismatch"

let rec infer (env: kind_env) (t: ty) (fresh: KindVarProvider.t): (substitution * kind) =
  match t with
  | TypeVar _ ->
      (empty_substitution, fresh ())
  | TypeConstructor id ->
      let kind = Map.find_exn env id in
      (empty_substitution, kind)
  | TypeApplication (lhs, rhs) ->
      let return_kind = fresh () in
      let (lhs_subs, lhs_kind) = infer env lhs fresh in
      let (rhs_subs, rhs_kind) = infer (apply_substitution_to_env lhs_subs env) rhs fresh in
      let rt_subs = unify (apply_substitution rhs_subs lhs_kind) (KindArrow (rhs_kind, return_kind)) in
      (substitution_compose rt_subs (substitution_compose rhs_subs lhs_subs), apply_substitution rt_subs return_kind)
  | _ -> failwith "couldn't infer"

let infer_kind (env: kind_env) (t: ty) =
  let fresh = KindVarProvider.create () in
  let (subs, k) = infer env t fresh in
  apply_substitution subs k

let%test_module "infer_kind" = (module struct
  let%expect_test "first try" =
    let env = Map.singleton (module String) "Int" KindType in
    let t = TypeConstructor "Int" in
    let kind = infer_kind env t in
    Stdio.print_endline (kind_to_string kind);
    [%expect {| Type |}]
  
  let%expect_test "arrow kind" =
    let env = Map.singleton (module String) "List" (KindArrow (KindType, KindType)) in
    let t = TypeConstructor "List" in
    let kind = infer_kind env t in
    Stdio.print_endline (kind_to_string kind);
    [%expect {| Type -> Type |}]
  
  let%expect_test "applied arrow" =
    let env =
      Map.of_alist_exn (module String) [
        ("Int", KindType);
        ("String", KindType);
        ("List", (KindArrow (KindType, KindType)));
        ("Result", (KindArrow (KindType, KindArrow (KindType, KindType))))
      ]
      in
    let t = TypeApplication (TypeApplication (TypeConstructor "Result", TypeConstructor "Int"), TypeConstructor "String") in
    let kind = infer_kind env t in
    Stdio.print_endline (kind_to_string kind);
    [%expect {| Type |}]
end)
