open Unnamed_programming_lang

let succeed t =
  let ts = Syntax.type_signature_to_string t in
  Stdio.printf "YAY: %s\n" ts

let fail lexbuf _ =
  Printf.fprintf stderr
    "At offset %d: syntax error.\n%!"
    (Sedlexing.lexeme_start lexbuf)

let parse lexbuf result =
  Parser.MenhirInterpreter.loop_handle
    succeed (fail lexbuf) (Lexer.makeSupplier lexbuf) result

let rec loop () =
  Stdio.printf "Enter Type Sig: ";
  match read_line () with
  | "stop" | "quit" | "exit" -> ()
  | s ->
    let lexbuf = Sedlexing.Utf8.from_string s in
    let (pos, _) = Sedlexing.lexing_positions lexbuf in
    parse lexbuf (Parser.Incremental.type_signature_eof pos);
    loop ()

let () = loop ()
