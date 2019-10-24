open Base
open Parser
open Ast
open Typechecker

let parse lexbuf =
  match (Parser.parse_expression lexbuf) with
  | Ok e -> 
    (match (Infer.infer_type Infer.Env.empty e) with
    | Ok t -> 
      let expr_str = Syntax.expression_to_string e in 
      let type_str = Syntax.type_signature_to_string t in
      Stdio.printf "%s\n%s\n" expr_str type_str
    | Error err -> Stdio.print_endline (Infer.err_to_string err)
    ) 

  | Error msg -> msg |> Stdio.print_endline

let rec loop () =
  Stdio.printf "Enter Exp: %!";
  match Stdio.In_channel.input_line Stdio.stdin with
  | None | Some "exit" | Some "quit" -> ()
  | Some s ->
    parse (Sedlexing.Utf8.from_string s);
    loop ()

let () = loop ()
