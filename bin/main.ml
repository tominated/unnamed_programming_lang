open Base
open Parser
open Ast

let parse lexbuf =
  Parser.parse_expression lexbuf
  |> Result.map ~f:Syntax.expression_to_string
  |> Result.ok_or_failwith

let rec loop () =
  Stdio.printf "Enter Exp: ";
  match read_line () with
  | "stop" | "quit" | "exit" -> ()
  | s ->
    parse (Sedlexing.Utf8.from_string s) |> Stdio.print_endline;
    loop ()

let () = loop ()
