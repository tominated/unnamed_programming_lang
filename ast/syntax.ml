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
  | TypeUnit
  (* () *)
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
  | TypeBindAtomic of type_signature
  | TypeBindVariant of (string * type_signature list) list

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
  | ExprValBinding of pattern * expression * expression
  (* let P1 = E1 in E2 *)
  | ExprTypeBinding of string * string list * type_binding * expression
  (* type L a = T in E *)
  | ExprFn of string * expression
  (* fn P1 -> E1 *)
  | ExprApply of expression * expression
  (* E1 E2 E3 *)
  | ExprInfix of expression * string * expression
  (* E1 op E2 *)
  | ExprMatch of expression * (pattern * expression) list
  (* match E1 with P2 -> E2 | ... *)
  | ExprIfElse of expression * expression * expression
  (* if E1 then E2 else E3 *)
  | ExprTuple of expression list
  (* (E0, E1) *)
  | ExprConstruct of string * expression list
  (* Constructor E0 E1 *)
  | ExprArray of expression list
  (* [E0, E1, E3] *)
  | ExprRecord of (string * expression) list * expression option
  (* { a: E0, b: E1 } or { ...E, a: E0, b: E1 } *)
  | ExprRecordAccess of expression * string
  (* E0.l *)
  | ExprConstraint of expression * type_signature
  (* E: T *)
  | ExprSequence of expression * expression
  (* E1; E2 *)

let rec type_signature_to_string ts =
  match ts.item with
  | TypeUnit -> "()"
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

let%test_module "constant_to_string" = (module struct
  let%expect_test "should print a string" =
    constant_to_string (ConstString "foo") |> Stdio.print_endline;
    [%expect {| foo |}]

  let%expect_test "should print a number" =
    constant_to_string (ConstNumber 123.) |> Stdio.print_endline;
    [%expect {| 123.000000 |}]
end)

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

(* Test function to create dummy locations for testing *)
let locate = fun p -> { item = p; location = (Lexing.dummy_pos, Lexing.dummy_pos) }

let%test_module "pattern_to_string" = (module struct
  let%expect_test "should print a constant" =
    let cStr = ConstString "foo" in
    let pattern = locate (PatternConstant cStr) in
    pattern_to_string pattern |> Stdio.print_endline;
    [%expect {| foo |}]

  let%expect_test "should print a tuple" =
    let foo = PatternConstant (ConstString "foo") in
    let bar = PatternConstant (ConstString "bar") in
    let pattern = locate (PatternTuple [locate foo; locate foo; locate bar]) in
    pattern_to_string pattern |> Stdio.print_endline;
    [%expect {| (foo, foo, bar) |}]

  let%expect_test "should print a constructor" =
    let foo = PatternConstant (ConstString "foo") in
    let bar = PatternConstant (ConstString "bar") in
    let ctor = PatternConstructor ("Test", [locate foo; locate bar]) in
    let pattern = locate ctor in
    pattern_to_string pattern |> Stdio.print_endline;
    [%expect {| Test foo bar |}]
end)


let type_binding_to_string tb =
  match tb with
  | TypeBindAtomic ts -> type_signature_to_string ts
  | TypeBindVariant xs ->
      let variant_to_string (n, ts) = Printf.sprintf "%s %s" n (String.concat ~sep:" " (List.map ~f:type_signature_to_string ts)) in
      String.concat ~sep:" | " (List.map ~f:variant_to_string xs)

let rec expression_to_string e =
  match e.item with
  | ExprUnit -> "()"
  | ExprConstant c -> constant_to_string c
  | ExprIdent id -> id
  | ExprValBinding (p, b, e) ->
      Printf.sprintf "let %s = %s in %s"
        (pattern_to_string p)
        (expression_to_string b)
        (expression_to_string e)
  | ExprTypeBinding (n, a, b, e) ->
      let args = String.concat (List.map ~f:(fun n -> Printf.sprintf " %s" n) a) in
      Printf.sprintf "type %s%s = %s in %s"
        n args (type_binding_to_string b) (expression_to_string e)
  | ExprFn (arg, e) ->
      Printf.sprintf "fn %s -> %s" arg (expression_to_string e)
  | ExprApply (e, arg) ->
      Printf.sprintf "%s %s" (expression_to_string e) (expression_to_string arg)
  | ExprInfix (lhs, op, rhs) ->
      Printf.sprintf "%s %s %s" (expression_to_string lhs) op (expression_to_string rhs)
  | ExprMatch (e, cs) ->
      let case_to_string (p, e) = Printf.sprintf "%s -> %s" (pattern_to_string p) (expression_to_string e) in
      let cases = String.concat ~sep:" | " (List.map ~f:case_to_string cs) in
      Printf.sprintf "match %s with %s" (expression_to_string e) cases
  | ExprIfElse (p, t, f) ->
      Printf.sprintf "if %s then %s else %s" (expression_to_string p) (expression_to_string t) (expression_to_string f)
  | ExprTuple es ->
      let exprs = String.concat ~sep:", " (List.map ~f:expression_to_string es) in
      Printf.sprintf "(%s)" exprs
  | ExprConstruct (n, es) ->
      let exprs = String.concat ~sep:" " (List.map ~f:expression_to_string es) in
      Printf.sprintf "%s %s" n exprs
  | ExprArray es ->
      es |> List.map ~f:expression_to_string |> String.concat ~sep:", " |> Printf.sprintf "[%s]"
  | ExprRecord (fs, b) ->
      let field_to_string (n, e) = Printf.sprintf "%s : %s" n (expression_to_string e) in
      let fields = fs |> List.map ~f:field_to_string |> String.concat ~sep:", " in
      let base = b |> Option.value_map ~default:"" ~f:(fun e -> Printf.sprintf " | %s" (expression_to_string e)) in
      Printf.sprintf "{ %s%s }" fields base
  | ExprRecordAccess (e1, n) -> Printf.sprintf "%s.%s" (expression_to_string e1) n
  | ExprConstraint (e, t) -> Printf.sprintf "%s : %s" (expression_to_string e) (type_signature_to_string t)
  | ExprSequence (e1, e2) -> Printf.sprintf "%s; %s" (expression_to_string e1) (expression_to_string e2)
