open Base

type name = String.t

[@@deriving sexp]
type literal =
  | Num of Float.t
  | Str of String.t

[@@deriving sexp]
type expr =
  | Val of literal
  | Var of name
  | Application of expr * expr List.t
  | Function of name list * expr
  | Let of name * expr * expr
  | RecordSelect of expr * name
  | RecordExtend of name * expr * expr
  | RecordEmpty
