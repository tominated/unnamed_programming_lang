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

let constant_to_string c =
  match c with
  | ConstNumber n -> Printf.sprintf "%f" n
  | ConstString s -> s

let rec pattern_to_string p =
  match p.item with
  | PatternAny -> "_"
  | PatternVar v -> v
  | PatternConstant c -> constant_to_string c
  | PatternTuple xs ->
      let patterns = String.concat ~sep:", " (List.map ~f:pattern_to_string xs) in
      Printf.sprintf "(%s)" patterns
  | PatternConstructor (n, xs) ->
      let patterns = String.concat ~sep:" " (List.map ~f:pattern_to_string xs) in
      Printf.sprintf "%s %s" n patterns
  | PatternRecord xs ->
      let field_to_string (n, p) = Printf.sprintf "%s: %s" n (pattern_to_string p) in
      let fields = String.concat ~sep:", " (List.map ~f:field_to_string xs) in
      Printf.sprintf "{ %s }" fields
  | PatternAlias (p, a) -> Printf.sprintf "%s as %s" (pattern_to_string p) a
  | PatternOr (a, b) -> Printf.sprintf "%s | %s" (pattern_to_string a) (pattern_to_string b)

let type_binding_to_string tb =
  match tb with
  | TypeDecAtomic ts -> type_signature_to_string ts
  | TypeDecVariant xs ->
      let variant_to_string (n, ts) = Printf.sprintf "%s %s" (n.item) (String.concat ~sep:" " (List.map ~f:type_signature_to_string ts)) in
      String.concat ~sep:" | " (List.map ~f:variant_to_string xs)

let rec expression_to_string e =
  match e.item with
  | ExprUnit -> "()"
  | ExprConstant c -> constant_to_string c
  | ExprIdent id -> id
  | ExprLet (p, b, e) ->
      Printf.sprintf "let %s = %s in\n%s"
        (pattern_to_string p)
        (expression_to_string b)
        (expression_to_string e)
  | ExprTypeDec (n, a, b, e) ->
      let args = String.concat (List.map ~f:(fun n -> Printf.sprintf " %s" n) a) in
      Printf.sprintf "type %s%s = %s in\n%s"
        n args (type_binding_to_string b) (expression_to_string e)
  | ExprFn (args, e) ->
      Printf.sprintf "fn %s -> %s" (String.concat ~sep:" " args) (expression_to_string e)
  | ExprApply (e, args) ->
      Printf.sprintf "%s %s"
        (expression_to_string e)
        (String.concat ~sep:" " (List.map ~f:expression_to_string args))
  | ExprMatch (e, cs) ->
      let case_to_string (p, e) = Printf.sprintf "%s -> %s" (pattern_to_string p) (expression_to_string e) in
      let cases = String.concat ~sep:" | " (List.map ~f:case_to_string cs) in
      Printf.sprintf "match %s with\n%s" (expression_to_string e) cases
  | ExprTuple es ->
      let exprs = String.concat ~sep:", " (List.map ~f:expression_to_string es) in
      Printf.sprintf "(%s)" exprs
  | ExprConstruct (n, es) ->
      let exprs = String.concat ~sep:" " (List.map ~f:expression_to_string es) in
      Printf.sprintf "%s %s" n exprs
  | ExprRecord (fs, ext) ->
      let fs_ = List.map ~f:(fun (name, e) -> Printf.sprintf "%s : %s" name (expression_to_string e)) fs in
      let fields = String.concat ~sep:", " fs_ in
      let ext_ = Option.value_map ~default:"" ~f:(Printf.sprintf " | %s") ext in
      Printf.sprintf "{ %s%s }" fields ext_
  | ExprRecordAccess (e1, n) -> Printf.sprintf "%s.%s" (expression_to_string e1) n
  | ExprConstraint (e, t) -> Printf.sprintf "%s : %s" (expression_to_string e) (type_signature_to_string t)
  | ExprSequence (e1, e2) -> Printf.sprintf "%s;\n%s" (expression_to_string e1) (expression_to_string e2)
