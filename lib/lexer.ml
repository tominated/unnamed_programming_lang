open Sedlexing
open Parser

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let ident = [%sedlex.regexp? Plus ('a'..'z'|'A'..'Z'|'0'..'9'|'_')]

(* let rec token lexbuf =
  match%sedlex lexbuf with
  | eof -> EOF
  | white_space -> token lexbuf
  | newline -> new_line lexbuf; token lexbuf;
  | ident -> IDENT (Utf8.lexeme lexbuf);
  | _ -> failwith ("Unrecognised input:" ^ (Utf8.lexeme lexbuf)) *)
