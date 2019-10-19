(** syntax.ml: The to-be type-checked AST that our language will be parsed in to *)

open Base

(** A value with its position in the source file *)
type 'a located = {
  item: 'a;
  location: Lexing.position * Lexing.position
}

(** A constant literal value *)
type constant =
  | ConstNumber of float
  | ConstString of string

(** A type signature *)
and type_signature = type_signature_ located
and type_signature_ =
  | TypeAny
  (* _ *)
  | TypeVar of string
  (* a *)
  | TypeIdent of string
  (* MyType *)
  | TypeConstructor of string * type_signature list
  (* Constructor T1 T2 *)
  | TypeArrow of type_signature * type_signature
  (* T1 -> T2 *)
  | TypeTuple of type_signature list
  (* (T1, T2) *)
  | TypeRecord of (string * type_signature) list * string option
  (* { a: T1, b: T2 | r } *)

(** A type that is defined before use *)
and type_binding =
  | TypeDecAtomic of type_signature
  | TypeDecVariant of (string located * type_signature list) list

(** A pattern to match a value against *)
and pattern = pattern_ located
and pattern_ =
  | PatternAny
  (* _ *)
  | PatternVar of string
  (* x *)
  | PatternConstant of constant
  (* 1 *)
  | PatternTuple of pattern list
  (* (P1, P2) *)
  | PatternConstructor of string * pattern list
  (* Constructor P1 P2 *)
  | PatternRecord of (string * pattern) list
  (* { l1: P1, l2: P2 } *)
  | PatternAlias of pattern * string
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
  | ExprIdent of string
  (* x *)
  | ExprLet of pattern * expression * expression
  (* let P1 = E1 in E2 *)
  | ExprTypeDec of string * string list * type_binding * expression
  (* type L a = T in E *)
  | ExprFn of string list * expression
  (* fn P1 -> E1 *)
  | ExprApply of expression * expression list
  (* E1 E2 E3 *)
  | ExprMatch of expression * (pattern * expression) list
  (* match E1 with P2 -> E2 | ... *)
  | ExprTuple of expression list
  (* (E0, E1) *)
  | ExprConstruct of string * expression list
  (* Constructor E0 E1 *)
  | ExprRecord of (string * expression) list * string option
  (* { a: E0, b: E1 } or { ...E, a: E0, b: E1 } *)
  | ExprRecordAccess of expression * string
  (* E0.l *)
  | ExprConstraint of expression * type_signature
  (* E: T *)
  | ExprSequence of expression * expression
  (* E1; E2 *)

let rec type_signature_to_string ts =
  match ts.item with
  | TypeAny -> "_"
  | TypeVar x -> x
  | TypeIdent x -> x
  | TypeConstructor (x, xs) ->
      let args = List.map ~f:type_signature_to_string xs in
      Printf.sprintf "%s %s" x (String.concat ~sep:" " args)
  | TypeArrow (a,b) -> Printf.sprintf "%s -> %s" (type_signature_to_string a) (type_signature_to_string b)
  | TypeTuple xs ->
      let vals = String.concat ~sep:", " (List.map ~f:type_signature_to_string xs) in
      Printf.sprintf "(%s)" vals
  | TypeRecord (fs, ext) ->
    let fs_ = List.map ~f:(fun (name, t) -> Printf.sprintf "%s : %s" name (type_signature_to_string t)) fs in
    let fields = String.concat ~sep:", " fs_ in
    let ext_ = Option.value_map ~default:"" ~f:(Printf.sprintf " | %s") ext in
    Printf.sprintf "{ %s%s }" fields ext_
