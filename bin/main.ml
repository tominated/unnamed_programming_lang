open Base
open Parser
open Ast

let parse lexbuf =
  match (Parser.parse_expression lexbuf) with
  | Ok e -> Syntax.expression_to_string e
  | Error msg -> msg

let rec loop () =
  Stdio.printf "Enter Exp: %!";
  match Stdio.In_channel.input_line Stdio.stdin with
  | None | Some "exit" | Some "quit" -> ()
  | Some s ->
    parse (Sedlexing.Utf8.from_string s) |> Stdio.print_endline;
    loop ()

let () = loop ()
