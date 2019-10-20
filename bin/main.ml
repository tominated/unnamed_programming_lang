open Unnamed_programming_lang

let succeed e =
  let ts = Syntax.expression_to_string e in
  Stdio.printf "YAY: %s\n" ts

let fail lexbuf _ =
  Printf.fprintf stderr
    "At offset %d: syntax error.\n%!"
    (Sedlexing.lexeme_start lexbuf)

let parse lexbuf result =
  Parser.MenhirInterpreter.loop_handle
    succeed (fail lexbuf) (Lexer.makeSupplier lexbuf) result

let rec loop () =
  Stdio.printf "Enter Exp: ";
  match read_line () with
  | "stop" | "quit" | "exit" -> ()
  | s ->
    let lexbuf = Sedlexing.Utf8.from_string s in
    let (pos, _) = Sedlexing.lexing_positions lexbuf in
    parse lexbuf (Parser.Incremental.parse_expression pos);
    loop ()

let () = loop ()
