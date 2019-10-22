%{
  open Ast.Syntax

  let located item location = { item = item; location = location }

  let fn_expr (args: string list) (body: expression) =
    Base.List.fold_right
      ~init:body
      ~f:(fun arg acc -> located (ExprFn (arg, acc)) body.location)
      args
    |> fun x -> x.item

  let apply_expr (fn: expression) (args: expression list) =
    Base.List.fold
      ~init:fn
      ~f:(fun acc arg -> located (ExprApply(acc, arg)) fn.location)
      args
    |> fun x -> x.item
%}

%token <string> LIDENT (* lowecase identifier *)
%token <string> UIDENT (* Uppercase identifier *)
%token <string> OPERATOR (* Operator identifier *)
%token <float> NUMBER
%token <string> STRING
%token FN LET TYPE IN IF THEN ELSE MATCH WITH AS
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token LPAREN "("
%token RPAREN ")"
%token ARROW "->"
%token EQUALS "="
%token PIPE "|"
%token DOT "."
%token COLON ":"
%token COMMA ","
%token UNDERSCORE "_"
%token EOF

%start parse_expression
%type <Ast.Syntax.expression> parse_expression

%start parse_type_signature
%type <Ast.Syntax.type_signature> parse_type_signature

%%

(** Location helper *)
%inline l(X):
  | x=X { located x $loc }

parse_expression:
  | e=l(expr) EOF { e }

(** Identifiers *)

val_ident: id=LIDENT | "(" id= OPERATOR ")" { id }

(** Patterns *)

(* A single field in a record *)
field_pattern:
  | id=val_ident "=" body=l(pattern) { (id, body) }
  | id=val_ident { (id, located (PatternVar id) $loc(id)) }

(* Any pattern that has unambigious grammar *)
atomic_pattern:
  | p=l(atomic_pattern) AS id=val_ident { PatternAlias (p, id) }
  | id=val_ident { PatternVar id }
  | id=UIDENT { PatternConstructor (id, []) }
  | c=constant { PatternConstant c }
  | "(" ps=l(pattern)+ ")" { PatternTuple ps }
  | "{" fields=field_pattern+ "}" { PatternRecord fields }

(* Any pattern to match a value against *)
pattern:
  | p=atomic_pattern { p }
  | id=UIDENT args=l(atomic_pattern)+ { PatternConstructor (id, args) }

(** Expressions *)

(* A literal *)
constant:
  | n=NUMBER { ConstNumber n }
  | s=STRING { ConstString s }

(* A case in a match expression *)
match_case:
  | "|" pat=l(pattern) "->" e=l(expr) { (pat, e) }

(* A field in a record *)
field_expr:
  | id=val_ident ":" e=l(expr) { (id, e) }

(* The base that a record is extending *)
record_expr_base:
  | "|" e=l(expr) { e }

(* A single variant definition *)
variant_def:
  | "|" id=UIDENT args=l(type_signature)* { (id, args) }

(* A type signature or variant to be bound *)
type_binding:
  | vs=variant_def+ { TypeBindVariant vs }
  | t=l(type_signature) { TypeBindAtomic t }

(* An unambiguous expression *)
atomic_expr:
  | id=val_ident { ExprIdent id }
  | c=constant { ExprConstant c }
  | e=l(atomic_expr) "." id=val_ident { ExprRecordAccess (e, id) }
  | "(" elems=separated_nontrivial_llist(",", l(expr)) ")" { ExprTuple elems }
  | "[" elems=separated_list(",", l(expr)) "]" { ExprArray elems }
  | "{" fields=separated_list(",", field_expr) base=record_expr_base? "}" {ExprRecord (fields, base) }

(* Function application and atomic expressions *)
application_expr:
  | e=atomic_expr { e }
  | id=UIDENT args=l(atomic_expr)* { ExprConstruct (id, args) }
  | e=l(atomic_expr) args=l(atomic_expr)+ { apply_expr e args }

(* Infix/function application, lambdas, atomic expressions *)
infix_expr:
  | e=application_expr { e }
  | FN args=val_ident+ "->" body=l(expr) { fn_expr args body }
  | lhs=l(application_expr) op=OPERATOR rhs=l(infix_expr) { ExprInfix (lhs, op, rhs) }

(* All expressions *)
expr:
  | e=infix_expr { e }
  | IF pred=l(expr) THEN t=l(expr) ELSE f=l(expr) { ExprIfElse (pred, t, f) }
  | MATCH e=l(expr) WITH cases=match_case+ { ExprMatch (e, cases) }
  | LET pat=l(atomic_pattern) "=" v=l(expr) IN e=l(expr) { ExprValBinding (pat, v, e) }
  | LET id=val_ident args=val_ident+ "=" body=l(expr) IN e=l(expr)
    {
      let pattern = located (PatternVar id) $loc(id) in
      let f = located (fn_expr args body) ($startpos(id), $endpos(body)) in
      ExprValBinding (pattern, f, e)
    }
  | TYPE id=UIDENT params=LIDENT* "=" t=type_binding IN e=l(expr) { ExprTypeBinding (id, params, t, e) }

(** Type Signatures *)

parse_type_signature:
  | t=l(type_signature) EOF { t }

type_signature:
  | t=atomic_type { t }
  | lhs=l(atomic_type) "->" rhs=l(type_signature) { TypeArrow (lhs, rhs) }

atomic_type:
  | "(" ts=separated_nontrivial_llist(",", l(atomic_type)) ")" { TypeTuple ts }
  | "{" fs=record_field_type_signature+ ext=record_extend_type_signature? "}"
      { TypeRecord (fs, ext) }
  | t=UIDENT ts=nonempty_list(l(type_signature)) { TypeConstructor (t, ts) }
  | "(" t=type_signature ")" { t }
  | LIDENT { TypeVar $1 }
  | UIDENT { TypeIdent $1 }

record_field_type_signature:
  | id=LIDENT ":" t=l(type_signature) { (id, t) }

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
