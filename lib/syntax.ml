(** syntax.ml: The to-be type-checked AST that our language will be parsed in to *)

open Base

(** A value with its position in the source file *)
type 'a located = { item: 'a; location: Lexing.position * Lexing.position }

type name = string

(** A constant literal value *)
type constant =
  | ConstNumber of float
  | ConstString of name

(** A type signature *)
and typedef = typedef_ located
and typedef_ =
  | TypeAny
  (* _ *)
  | TypeVar of name
  (* a *)
  | TypeIdent of name
  (* MyType *)
  | TypeConstructor of name * typedef list
  (* Constructor T1 T2 *)
  | TypeArrow of typedef * typedef
  (* T1 -> T2 *)
  | TypeTuple of typedef list
  (* (T1, T2) *)
  | TypeRecord of (name * typedef) list * name option
  (* { a: T1, b: T2 | r } *)

(** A pattern to match a value against *)
and pattern = pattern_ located
and pattern_ =
  | PatternAny
  (* _ *)
  | PatternVar of name
  (* x *)
  | PatternConstant of constant
  (* 1 *)
  | PatternTuple of pattern list
  (* (P1, P2) *)
  | PatternConstructor of name * pattern list
  (* Constructor P1 P2 *)
  | PatternRecord of (name * pattern) list
  (* { l1: P1, l2: P2 } *)
  | PatternAlias of pattern * name
  (* P1 as l *)
  | PatternOr of pattern * pattern
  (* P1 | P2 *)

(** An expression that may evaluate to a value *)
and expression = expression_ located
and expression_ =
  | ExprUnit
  (** () *)
  | ExprConstant of constant
  (* 1, "test" *)
  | ExprIdent of name
  (* x *)
  | ExprLet of pattern * expression * expression
  (* let P1 = E1 in E2 *)
  | ExprTypeDec of name * name list * typedef * expression
  (* type L a = T in E *)
  | ExprFn of name list * expression
  (* fn P1 -> E1 *)
  | ExprApply of expression * expression list
  (* E1 E2 E3 *)
  | ExprMatch of expression * (pattern * expression) list
  (* match E1 with P2 -> E2 | ... *)
  | ExprTuple of expression list
  (* (E0, E1) *)
  | ExprConstruct of name * expression list
  (* Constructor E0 E1 *)
  | ExprRecord of (name * expression) list * name option
  (* { a: E0, b: E1 } or { ...E, a: E0, b: E1 } *)
  | ExprRecordAccess of expression * name
  (* E0.l *)
  | ExprConstraint of expression * typedef
  (* E: T *)
  | ExprSequence of expression * expression
  (* E1; E2 *)
