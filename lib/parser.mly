%{
  open Syntax

  let located item location = { item = item; location = location }
%}

%token <string> LIDENT (* lowecase identifier *)
%token <string> UIDENT (* Uppercase identifier *)
%token <float> NUMBER
%token <string> STRING
%token FN LET IN MATCH WITH AS
%token LBRACE RBRACE LPAREN RPAREN
%token SEP
%token ARROW EQUALS PIPE DOT COLON COMMA UNDERSCORE
%token EOF

%start expr_eof
%type <Syntax.expression> expr_eof

%start typedef
%type <Syntax.typedef> typedef

%%

expr_eof:
  | expr EOF { $1 }

expr:
  | expr_ SEP { located $1 $loc }

expr_:
  /* () */
  | LPAREN RPAREN { ExprUnit }

  /* 1, "Hello" */
  | constant { ExprConstant $1 }

  /* myId */
  | LIDENT { ExprIdent $1 }

  /* MyId */
  | UIDENT { ExprIdent $1 }

  /* (1, "a") */
  | LPAREN xs=separated_nonempty_list(COMMA, expr) RPAREN { ExprTuple xs }

  /* {} */
  | LBRACE RBRACE { ExprRecord ([], None) }

  /* { a: "hello", b: 1 | r } */
  | LBRACE
    xs=separated_nonempty_list(COMMA, record_field)
    ext=record_extends?
    RPAREN
      { ExprRecord (xs, ext) }

  /* myRecord.fieldName */
  | e=expr_ DOT id=LIDENT { ExprRecordAccess (located e $loc(e), id) }

  /* (...) */
  | LPAREN e=expr_ RPAREN { e }

  /* fn x -> x + 1 */
  | FN args=LIDENT+ ARROW fnbody=expr { ExprFn(args, fnbody) }

  /* match x with
      | 1 -> "yay"
      | _ -> "nay"
  */
  | MATCH e=expr WITH cases=match_case+ { ExprMatch(e, cases) }

  /* let x = 1 in ... */
  /* let x y x = x + y + z in ... */
  | let_binding { $1 }

constant:
  | NUMBER { ConstNumber($1) }
  | STRING { ConstString($1) }

record_field:
  | id=LIDENT e=preceded(COLON, expr) { (id, e) }
  | id=LIDENT { (id, located (ExprIdent id) $loc)}

record_extends:
  | PIPE id=LIDENT { id }

pattern:
  | pattern_ { located $1 $loc }

pattern_:
  | UNDERSCORE { PatternAny }
  | constant { PatternConstant $1 }
  | LIDENT { PatternVar $1 }
  | p=pattern AS id=LIDENT { PatternAlias (p, id) }
  | LPAREN xs=separated_nonempty_list(COMMA, pattern) RPAREN
      { PatternTuple xs }
  | LBRACE xs=separated_nonempty_list(COMMA, record_pattern) RBRACE
      { PatternRecord xs }

record_pattern:
  | id=LIDENT { (id, located (PatternVar id) $loc) }
  | id=LIDENT COLON p=pattern { (id, p) }

match_case:
  | PIPE p=pattern ARROW e=expr { (p, e) }

let_binding:
  | LET id=pattern EQUALS binding=expr IN body=expr
      { ExprLet(id, binding, body) }

  | LET id=LIDENT args=LIDENT+ EQUALS fnbody=expr IN body=expr
      {
        let name = located (PatternVar id) $loc(id) in
        let fn_loc = ($startpos(args), $endpos(fnbody)) in
        let fn = located (ExprFn (args, fnbody)) fn_loc in
        ExprLet(name, fn, body)
      }

typedef:
  | typedef_ { located $1 $loc }

typedef_:
  | atomic_type_ { $1 }
  | LPAREN xs=separated_nonempty_list(COMMA, atomic_type) RPAREN { TypeTuple xs }
  | LBRACE xs=separated_nonempty_list(COMMA, record_field_typedef) ext=record_extend_typedef?
      { TypeRecord (xs, ext) }

atomic_type:
  | atomic_type_ { located $1 $loc }

atomic_type_:
  | LPAREN t=typedef_ RPAREN { t }
  | UNDERSCORE { TypeAny }
  | LIDENT { TypeVar $1 }
  | UIDENT { TypeIdent $1 }
  | UIDENT nonempty_list(atomic_type) { }

record_field_typedef:
  | id=LIDENT COLON t=atomic_type { (id, t) }

record_extend_typedef:
  | PIPE id=LIDENT { id }

(*
expr_eof:
  | expr EOF { $1 }

expr:
  | simple_expr { $1 }
  | block_expr { $1 }
  | FN nonempty_list(pattern) ARROW expr { FunctionExpr($2, $4) }
  | LET pattern EQUALS expr IN expr { LetExpr($2, $4, $6) }
  | LET IDENT nonempty_list(pattern) EQUALS expr IN expr { LetExpr(VarPattern($2), FunctionExpr($3, $5), $7) }
  | simple_expr nonempty_list(simple_expr) { ApplicationExpr($1, $2) }
  | simple_expr LPAREN RPAREN { ApplicationExpr($1, []) }
  | MATCH simple_expr WITH nonempty_list(match_entry) { MatchExpr($2, $4) }

simple_expr:
  | literal { ValExpr($1) }
  | IDENT { VarExpr $1 }
  | LPAREN RPAREN { UnitExpr }
  | LPAREN expr RPAREN { $2 }
  | LBRACE RBRACE { RecordEmptyExpr }
  | LBRACE record_fields RBRACE { fold_record_fields $2 RecordEmptyExpr }
  | LBRACE record_fields COMMA ELLIPSIS simple_expr RBRACE { fold_record_fields $2 $5 }
  | simple_expr DOT IDENT { RecordSelectExpr($1, $3) }

block_expr:
  | BEGIN separated_list(SEP, expr) END { BlockExpr($2) }

literal:
  | NUMBER { NumberLiteral($1) }
  | STRING { StringLiteral($1) }

record_fields:
  | separated_nonempty_list(COMMA, record_field) { $1 }

record_field:
  | IDENT COLON expr { ($1, $3) }

match_entry:
  | PIPE pattern ARROW simple_expr { ($2, $4) }

pattern:
  | UNDERSCORE { WildcardPattern }
  | literal { LiteralPattern($1) }
  | IDENT { VarPattern($1) }
  | LBRACE record_pattern RBRACE { RecordPattern($2) }

record_pattern:
  | ELLIPSIS IDENT { RestRPattern($2) }
  | IDENT COMMA record_pattern { FieldRPattern($1, VarPattern($1), $3) }
  | IDENT { FieldRPattern($1, VarPattern($1), NilRPattern) }
  | IDENT COLON IDENT COMMA record_pattern { FieldRPattern($1, VarPattern($3), $5) }
  | IDENT COLON IDENT { FieldRPattern($1, VarPattern($3), NilRPattern) }
*)
