%{
  open Syntax

  let located item location = { item = item; location = location }
%}

%token <string> LIDENT (* lowecase identifier *)
%token <string> UIDENT (* Uppercase identifier *)
%token <float> NUMBER
%token <string> STRING
%token FN LET TYPE IN MATCH WITH AS
%token LBRACE "{"
%token RBRACE "}"
%token LPAREN "("
%token RPAREN ")"
%token ARROW "->"
%token EQUALS "="
%token PIPE "|"
%token DOT "."
%token COLON ":"
%token COMMA ","
%token UNDERSCORE "_"
%token SEMI
%token EOF

%start expr_eof
%type <Syntax.expression> expr_eof

%start type_signature_eof
%type <Syntax.type_signature> type_signature_eof

%%

expr_eof:
  | expr EOF { $1 }

expr:
  | expr_ SEMI { located $1 $loc }

expr_:
  /* () */
  | "(" ")" { ExprUnit }

  /* 1, "Hello" */
  | constant { ExprConstant $1 }

  /* myId */
  | LIDENT { ExprIdent $1 }

  /* MyId */
  | UIDENT { ExprIdent $1 }

  /* (1, "a") */
  | "(" xs=separated_nonempty_list(",", expr) ")" { ExprTuple xs }

  /* {} */
  | "{" "}" { ExprRecord ([], None) }

  /* { a: "hello", b: 1 | r } */
  | "{"
    xs=separated_nonempty_list(",", record_field)
    ext=record_extends?
    "}"
      { ExprRecord (xs, ext) }

  /* myRecord.fieldName */
  | e=expr_ "." id=LIDENT { ExprRecordAccess (located e $loc(e), id) }

  /* (...) */
  | "(" e=expr_ ")" { e }

  /* fn x -> x + 1 */
  | FN args=LIDENT+ "->" fnbody=expr { ExprFn(args, fnbody) }

  /* match x with
      | 1 -> "yay"
      | _ -> "nay"
  */
  | MATCH e=expr WITH cases=match_case+ { ExprMatch(e, cases) }

  /* let x = 1 in ... */
  /* let x y x = x + y + z in ... */
  | let_binding { $1 }

  /* let T = Number in ... */
  /* let T = { x: Option Boolean } in ... */
  /* let Option x = | None | Some x in ... */
  | type_binding { $1 }

constant:
  | NUMBER { ConstNumber($1) }
  | STRING { ConstString($1) }

record_field:
  | id=LIDENT e=preceded(":", expr) { (id, e) }
  | id=LIDENT { (id, located (ExprIdent id) $loc)}

record_extends:
  | "|" id=LIDENT { id }

pattern:
  | pattern_ { located $1 $loc }

pattern_:
  | "_" { PatternAny }
  | constant { PatternConstant $1 }
  | LIDENT { PatternVar $1 }
  | p=pattern AS id=LIDENT { PatternAlias (p, id) }
  | "(" xs=separated_nonempty_list(",", pattern) ")"
      { PatternTuple xs }
  | "{" xs=separated_nonempty_list(",", record_pattern) "}"
      { PatternRecord xs }

record_pattern:
  | id=LIDENT { (id, located (PatternVar id) $loc) }
  | id=LIDENT ":" p=pattern { (id, p) }

match_case:
  | "|" p=pattern "->" e=expr { (p, e) }

let_binding:
  | LET id=pattern "=" binding=expr IN body=expr
      { ExprLet(id, binding, body) }

  | LET id=LIDENT args=LIDENT+ "=" fnbody=expr IN body=expr
      {
        let name = located (PatternVar id) $loc(id) in
        let fn_loc = ($startpos(args), $endpos(fnbody)) in
        let fn = located (ExprFn (args, fnbody)) fn_loc in
        ExprLet(name, fn, body)
      }

type_binding:
  | TYPE id=UIDENT args=LIDENT* "=" binding=type_binding_body IN body=expr
      { ExprTypeDec (id, args, binding, body) }

type_binding_body:
  | variant_def+ { TypeDecVariant $1 }
  | type_signature { TypeDecAtomic $1 }

variant_def:
  | "|" id=UIDENT args=atomic_type* { (located id $loc(id), args) }

type_signature_eof:
  | type_signature EOF { $1 }

type_signature:
  | type_signature_ { located $1 $loc($1) }

type_signature_:
  | fn_type { $1 }

fn_type:
  | t=tuple_type_ { t }
  | lhs=tuple_type "->" rhs=type_signature { TypeArrow (lhs, rhs) }

tuple_type:
  | t=tuple_type_ { located t $loc }

tuple_type_:
  | t=record_type_ { t }
  | "(" ts=separated_nontrivial_llist(",", record_type) ")" { TypeTuple ts }

record_type:
  | t=record_type_ { located t $loc }

record_type_:
  | t=construct_type { t }
  | "{"
    fs=record_field_type_signature+
    ext=record_extend_type_signature?
    "}"
      { TypeRecord (fs, ext) }

construct_type:
  | t=atomic_type_ { t }
  | t=UIDENT ts=nonempty_list(atomic_type) { TypeConstructor (t, ts) }

atomic_type:
  | t=atomic_type_ { located t $loc }

atomic_type_:
  | "(" t=fn_type ")" { t }
  | "_" { TypeAny }
  | LIDENT { TypeVar $1 }
  | UIDENT { TypeIdent $1 }

record_field_type_signature:
  | id=LIDENT ":" t=atomic_type { (id, t) }

record_extend_type_signature:
  | "|" id=LIDENT { id }

(* STOLEN FROM OCAML SOURCE *)
(* https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly *)

(* [reversed_separated_nontrivial_llist(separator, X)] recognizes a list of at
   least two [X]s, separated with [separator]s, and produces an OCaml list in
   reverse order -- that is, the last element in the input text appears first
   in this list. Its definition is left-recursive. *)
reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

(* [separated_nontrivial_llist(separator, X)] recognizes a list of at least
   two [X]s, separated with [separator]s, and produces an OCaml list in direct
   order -- that is, the first element in the input text appears first in this
   list. *)

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }
