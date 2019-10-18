open Unnamed_programming_lang

let testBuf = Sedlexing.Utf8.from_string "let x = { hello: 1 } in Hello 1;"
let supplier = Lexer.makeSupplier testBuf

let succeed _ =
  Printf.printf "Well something worked..."

let fail lexbuf _ =
  Printf.fprintf stderr
    "At offset %d: syntax error.\n%!"
    (Sedlexing.lexeme_start lexbuf)

let loop lexbuf result =
  Parser.MenhirInterpreter.loop_handle
    succeed (fail lexbuf) supplier result

let () =
  let (pos, _) = Sedlexing.lexing_positions testBuf in
  loop testBuf (Parser.Incremental.expr_eof pos)
