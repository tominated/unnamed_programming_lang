(** ast.ml: The to-be type-checked AST that our language will be parsed in to *)

open Base

type name = String.t

[@@deriving sexp]
type literal =
  | NumberLiteral of Float.t
  | StringLiteral of String.t

[@@deriving sexp]
type expr =
  | UnitExpr
  | BlockExpr of expr List.t (** A sequence of expressions *)
  | ValExpr of literal (** Literal value *)
  | VarExpr of name (** Variable reference *)
  | ApplicationExpr of expr * expr List.t (** Function application *)
  | FunctionExpr of pattern list * expr (** Function definition *)
  | LetExpr of pattern * expr * expr (** Let binding *)
  | RecordSelectExpr of expr * name (** Record field selection *)
  | RecordExtendExpr of name * expr * expr (** Add field to record *)
  | RecordEmptyExpr (** Empty Record *)
  | MatchExpr of expr * (pattern * expr) List.t (** Pattern matching *)

[@@deriving sexp]
and pattern =
  | WildcardPattern (** Match anything without binding *)
  | LiteralPattern of literal (** Match a literal value *)
  | VarPattern of name (** Match anything and bind to variable *)
  | RecordPattern of record_pattern (** Match record structure *)

[@@deriving sexp]
and record_pattern =
  | NilRPattern (** end of record pattern *)
  | RestRPattern of name (** the rest of the values with a label *)
  | FieldRPattern of name * pattern * record_pattern (** a field, pattern and the rest of the pattern *)