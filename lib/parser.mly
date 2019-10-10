%{
  open Ast

  let fold_record_fields fields start =
    List.fold_left
      (fun acc (name, expr) -> RecordExtendExpr(name, expr, acc))
      start
      fields
%}

%token <string> IDENT
%token <float> NUMBER
%token <string> STRING
%token FN LET IN MATCH
%token LPAREN RPAREN LBRACE RBRACE
%token OPENBLOCK CLOSEBLOCK SEP
%token ARROW EQUALS DOT ELLIPSIS COLON COMMA UNDERSCORE
%token EOF

%start expr_eof
%type <Ast.expr> expr_eof

%%

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
  | MATCH simple_expr IN nonempty_list(match_entry) { MatchExpr($2, $4) }

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
  | OPENBLOCK separated_list(SEP, expr) CLOSEBLOCK { BlockExpr($2) }

literal:
  | NUMBER { NumberLiteral($1) }
  | STRING { StringLiteral($1) }

record_fields:
  | separated_nonempty_list(COMMA, record_field) { $1 }

record_field:
  | IDENT COLON expr { ($1, $3) }

match_entry:
  | pattern ARROW expr { ($1, $3) }

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