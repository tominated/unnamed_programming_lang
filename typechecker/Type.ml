open Base
open Ast.Syntax

type t = type_signature

let rec free_type_vars = function
  | TypeVar a -> Set.singleton (module String) a
  | TypeArrow (t1, t2) ->
      Set.union (free_type_vars t1) (free_type_vars t2)
  | TypeTuple ts ->
      ts
      |> List.map ~f:free_type_vars
      |> Set.union_list (module String)
  | TypeRecord t -> free_type_vars t
  | TypeRowEmpty -> Set.empty (module String)
  | TypeRowExtend (_, t, r) ->
      Set.union (free_type_vars r) (free_type_vars t)
  | _ -> Set.empty (module String)

let to_string = type_signature_to_string
