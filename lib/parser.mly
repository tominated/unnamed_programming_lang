%{
  open Ast

  let fold_record_fields fields start =
    List.fold_left
      (fun acc (name, expr) -> RecordExtend(name, expr, acc))
      start
      fields
%}

%token <string> IDENT
%token <float> NUMBER
%token <string> STRING
%token FN LET IN
%token LPAREN RPAREN LBRACE RBRACE
%token ARROW EQUALS DOT ELLIPSIS COLON COMMA
%token EOF

%start expr_eof
%type <Ast.expr> expr_eof

%%

expr_eof:
  | expr EOF { $1 }

expr:
  | simple_expr { $1 }
  | FN list(IDENT) ARROW expr { Function($2, $4) }
  | LET IDENT EQUALS expr IN expr { Let($2, $4, $6) }
  | LET IDENT nonempty_list(IDENT) EQUALS expr IN expr { Let($2, Function($3, $5), $7) }
  | simple_expr nonempty_list(simple_expr) { Application($1, $2) }
  | simple_expr LPAREN RPAREN { Application($1, []) }

simple_expr:
  | NUMBER { Val(Num $1) }
  | STRING { Val(Str $1) }
  | IDENT { Var $1 }
  | LPAREN expr RPAREN { $2 }
  | LBRACE RBRACE { RecordEmpty }
  | LBRACE record_fields RBRACE { fold_record_fields $2 RecordEmpty }
  | LBRACE record_fields COMMA ELLIPSIS simple_expr RBRACE { fold_record_fields $2 $5 }
  | simple_expr DOT IDENT { RecordSelect($1, $3) }

record_fields:
  | separated_nonempty_list(COMMA, record_field) { $1 }

record_field:
  | IDENT COLON expr { ($1, $3) }

