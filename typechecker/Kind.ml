open Base

type t =
  | KindType
  | KindRow
  | KindArrow of t * t
  | KindVar of string

let rec to_string (k: t) =
  match k with
  | KindType -> "Type"
  | KindRow -> "Row"
  | KindArrow (lhs, rhs) -> Printf.sprintf "%s -> %s" (to_string lhs) (to_string rhs)
  | KindVar v -> v

let rec free_kind_vars (k: t) : Set.M(String).t =
  match k with
  | KindType | KindRow -> Set.empty (module String)
  | KindArrow (l, r) -> Set.union (free_kind_vars l) (free_kind_vars r)
  | KindVar v -> Set.singleton (module String) v

let is_scalar (k: t) : bool =
  match k with
  | KindType -> true
  | _ -> false
