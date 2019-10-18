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
  let (s, e) = L.lexing_positions lexbuf in
  let debug = Printf.sprintf " %i-%i:" s.pos_cnum e.pos_cnum in
  p debug;
  match%sedlex lexbuf with
  | eof -> p "eof,"; EOF
  | Plus white_space -> p "ws,"; token lexbuf
  | newline -> p "nl,"; L.new_line lexbuf; token lexbuf
  | '{' -> p "{,"; LBRACE
  | '}' -> p "},"; RBRACE
  | '(' -> p "(,"; LPAREN
  | ')' -> p "),"; RPAREN
  | '.' -> p ".,"; DOT
  | ',' -> p ",,"; COMMA
  | ':' -> p ":,"; COLON
  | '|' -> p "|,"; PIPE
  | '_' -> p "|,"; UNDERSCORE
  | '=' -> p "=,"; EQUALS
  | ';' -> p ";,"; SEMI
  | "->" -> p "->,"; ARROW
  | "fn" -> p "fn,"; FN
  | "let" -> p "let,"; LET
  | "type" -> p "type,"; TYPE
  | "in" -> p "in,"; IN
  | "match" -> p "match,"; MATCH
  | "with" -> p "with,"; WITH
  | "as" -> p "as,"; AS
  | number -> p "num,"; NUMBER (Float.of_string (L.Utf8.lexeme lexbuf))
  | lident -> p "lident,"; LIDENT (L.Utf8.lexeme lexbuf)
  | uident -> p "uident,"; UIDENT (L.Utf8.lexeme lexbuf)
  | '"' -> p "string,"; read_string (Buffer.create 32) lexbuf
  | _ ->
    let (s, e) = L.lexing_positions lexbuf in
    p "\n";
    failwith (Printf.sprintf "Something went wrong at character %i-%i" s.pos_cnum e.pos_cnum)

let makeSupplier = L.with_tokenizer token
