(*
open Base

open Result.Let_syntax

type err =
  | KindNotFound of string
  | KindMismatch of Kind.t * Kind.t
  | InfiniteKind
  | Unimplemented

let err_to_string (e: err) =
  match e with
  | KindNotFound id -> Printf.sprintf "Type %s not found" id
  | KindMismatch (a, b) ->
      Printf.sprintf "Cannot unify '%s' with '%s'"
        (Kind.to_string a)
        (Kind.to_string b)
  | InfiniteKind -> "Infinite Kind"
  | Unimplemented -> "Unimplemented"


module KVarProvider = struct
  type t = unit -> Kind.t

  let create (): t =
    let last_id = ref 0 in
    fun () ->
      let id = !last_id in
      last_id := id + 1;
      KindVar (Printf.sprintf "k%d" id)
end

let var_bind (v: string) (k: Kind.t) =
  match (v, k) with
  | v, KindVar v_ when String.equal v v_ -> Ok KindSubst.null
  | v, _ when Set.mem (Kind.free_kind_vars k) v ->
      Error InfiniteKind
  | _ -> Ok (KindSubst.singleton v k)

let rec unify (k1: Kind.t) (k2: Kind.t) =
  match (k1, k2) with
  | KindVar v1, KindVar v2 when String.equal v1 v2 -> Ok KindSubst.null
  | KindVar v, k | k, KindVar v -> var_bind v k
  | KindType, KindType -> Ok KindSubst.null
  | KindArrow (l1, r1), KindArrow(l2, r2) ->
      let%bind s1 = unify l1 l2 in
      let%map s2 = unify (KindSubst.kind_apply s1 r1) (KindSubst.kind_apply s1 r2) in
      KindSubst.compose s2 s1
  | _ -> Error (KindMismatch (k1, k2))

let rec infer (env: KindEnv.t) (t: Type.t) (new_kvar: KVarProvider.t) =
  match t.item with
  | TypeVar _ -> Ok (KindSubst.null, new_kvar ())
  | TypeIdent id ->
      KindEnv.lookup env id
      |> Option.map ~f:(fun k -> (KindSubst.null, k))
      |> Result.of_option ~error:(KindNotFound id)
  | TypeConstructor (c, args) ->
    let return_kind = new_kvar () in
    let%bind c_infer = infer env c new_kvar in
    List.fold_result
      ~init:c_infer
      ~f:(fun (c_subs, c_kind) arg ->
        let%bind (arg_subs, arg_kind) = infer (KindSubst.env_apply c_subs env) arg new_kvar in
        let%bind return_subs = unify (KindSubst.kind_apply arg_subs c_kind) (KindArrow (arg_kind, return_kind)) in
        Ok (
          KindSubst.compose return_subs (KindSubst.compose arg_subs c_subs),
          KindSubst.kind_apply return_subs return_kind
        )
      )
      args

  | TypeUnit | TypeArrow _ | TypeTuple _ | TypeRecord _ -> Ok (KindSubst.null, KindType)
  | TypeRowEmpty | TypeRowExtend _ -> Ok (KindSubst.null, KindRow)

let infer_kind (env: KindEnv.t) (t: Type.t) =
  let new_kvar = KVarProvider.create () in
  let%map (subs, k) = infer env t new_kvar in
  KindSubst.kind_apply subs k

let%test_module "infer_kind" = (module struct
  open Ast.Syntax

  let locate (p: 'a) : 'a located = { item = p; location = (Lexing.dummy_pos, Lexing.dummy_pos) }

  let%expect_test "first try" =
    let env = KindEnv.singleton "Int" KindType in
    let t = locate (TypeIdent "Int") in
    infer_kind env t
    |> Result.map ~f:Kind.to_string
    |> Result.iter ~f:Stdio.print_endline;
    [%expect {| Type |}]

  let%expect_test "arrow kind" =
    let env = KindEnv.singleton "List" (KindArrow (KindType, KindType)) in
    let t = locate (TypeIdent "List") in
    infer_kind env t
    |> Result.map ~f:Kind.to_string
    |> Result.iter ~f:Stdio.print_endline;
    [%expect {| Type -> Type |}]

  let%expect_test "applied arrow" =
    let env =
      KindEnv.of_alist_exn [
        ("Int", KindType);
        ("String", KindType);
        ("List", (KindArrow (KindType, KindType)));
        ("Result", (KindArrow (KindType, KindArrow (KindType, KindType))))
      ]
      in
    let result_t = locate (TypeIdent "Result") in
    let int_t = locate (TypeIdent "Int") in
    let string_t = locate (TypeIdent "String") in
    let t = locate (TypeConstructor (result_t, [int_t; string_t])) in
    infer_kind env t
    |> Result.map ~f:Kind.to_string
    |> Result.iter ~f:Stdio.print_endline;
    [%expect {| Type |}]
end)
*)
