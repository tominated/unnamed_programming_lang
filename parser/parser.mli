open Base

val parse_expression: Sedlexing.lexbuf -> (Ast.Syntax.expression, string) Result.t
val parse_type_signature: Sedlexing.lexbuf -> (Ast.Syntax.type_signature, string) Result.t
