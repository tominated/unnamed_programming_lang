open Base
open Ast.Syntax

type t = scheme

let free_type_vars (scheme: t): Set.M(String).t =
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
