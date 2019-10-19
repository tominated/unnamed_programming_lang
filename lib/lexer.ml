open Base
open Parser

module L = Sedlexing
module I = Parser.MenhirInterpreter

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let ident_char = [%sedlex.regexp? ('a'..'z'|'A'..'Z'|'0'..'9'|'_')]
let number = [%sedlex.regexp? Plus ('0'..'9'), Opt ('.', Plus '0'..'9')]
let lident = [%sedlex.regexp? 'a'..'z', Star ident_char]
let uident = [%sedlex.regexp? 'A'..'Z', Star ident_char]
let operator = [%sedlex.regexp? Chars "!@#$%^&*-_=+<>?/\\~"]

let rec read_string buf lexbuf =
  match%sedlex lexbuf with
  | '"' -> STRING (Buffer.contents buf)
  | '\\', '/' -> Buffer.add_char buf '/'; read_string buf lexbuf
  | '\\', '\\' -> Buffer.add_char buf '\\'; read_string buf lexbuf
  | '\\', 'b' -> Buffer.add_char buf '\b'; read_string buf lexbuf
  | '\\', 'f' -> Buffer.add_char buf '\012'; read_string buf lexbuf
  | '\\', 'n' -> Buffer.add_char buf '\n'; read_string buf lexbuf
  | '\\', 'r' -> Buffer.add_char buf '\r'; read_string buf lexbuf
  | '\\', 't' -> Buffer.add_char buf '\t'; read_string buf lexbuf
  | '\\', '"' -> Buffer.add_char buf '\"'; read_string buf lexbuf
  | '\\', '\'' -> Buffer.add_char buf '\''; read_string buf lexbuf
  | Plus (Compl ('\\' | '"')) -> Buffer.add_string buf (L.Utf8.lexeme lexbuf); read_string buf lexbuf
  | eof -> failwith "String not terminated"
  | _ -> failwith ("Illegal string character:" ^ (L.Utf8.lexeme lexbuf))

let p = Stdio.Out_channel.output_string Stdio.stdout

let rec token lexbuf =
  match%sedlex lexbuf with
  | eof -> EOF
  | Plus white_space -> token lexbuf
  | newline -> L.new_line lexbuf; token lexbuf
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '.' -> DOT
  | ',' -> COMMA
  | ':' -> COLON
  | '|' -> PIPE
  | '_' -> UNDERSCORE
  | '=' -> EQUALS
  | ';' -> SEMI
  | "->" -> ARROW
  | "fn" -> FN
  | "let" -> LET
  | "type" -> TYPE
  | "in" -> IN
  | "match" -> MATCH
  | "with" -> WITH
  | "as" -> AS
  | number -> NUMBER (Float.of_string (L.Utf8.lexeme lexbuf))
  | lident -> LIDENT (L.Utf8.lexeme lexbuf)
  | uident -> UIDENT (L.Utf8.lexeme lexbuf)
  | '"' -> read_string (Buffer.create 32) lexbuf
  | _ ->
    let (s, e) = L.lexing_positions lexbuf in
    p "\n";
    failwith (Printf.sprintf "Something went wrong at character %i-%i" s.pos_cnum e.pos_cnum)

let makeSupplier = L.with_tokenizer token
