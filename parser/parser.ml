open Base

let fail lexbuf _ =
  lexbuf 
  |> Sedlexing.lexeme_start
  |> Printf.sprintf "Syntax error at offset %d"
  |> Result.fail

let success e = Result.Ok e

let parse_expression lexbuf =
  let (first_pos, _) = Sedlexing.lexing_positions lexbuf in
  let checkpoint = Expression_parser.Incremental.parse_expression first_pos in
  let supplier = Lexer.makeSupplier lexbuf in
  Expression_parser.MenhirInterpreter.loop_handle
    success (fail lexbuf) supplier checkpoint

let parse_type_signature lexbuf =
  let (first_pos, _) = Sedlexing.lexing_positions lexbuf in
  let checkpoint = Expression_parser.Incremental.parse_type_signature first_pos in
  let supplier = Lexer.makeSupplier lexbuf in
  Expression_parser.MenhirInterpreter.loop_handle
    success (fail lexbuf) supplier checkpoint
