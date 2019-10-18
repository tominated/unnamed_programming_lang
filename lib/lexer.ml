open Base
open Parser

module L = Sedlexing
module I = Parser.MenhirInterpreter

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]

let ident = [%sedlex.regexp? Plus ('a'..'z'|'A'..'Z'|'0'..'9'|'_')]
let number = [%sedlex.regexp? Plus ('0'..'9'), Opt ('.', Plus '0'..'9')]
let lident = [%sedlex.regexp? 'a'..'z', ident]
let uident = [%sedlex.regexp? 'A'..'Z', ident]
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


let rec token lexbuf =
  match%sedlex lexbuf with
  | eof -> EOF
  | white_space -> token lexbuf
  | newline -> L.new_line lexbuf; token lexbuf
  | number -> NUMBER (Float.of_string (L.Utf8.lexeme lexbuf))
  | Plus lident -> LIDENT (L.Utf8.lexeme lexbuf)
  | Plus uident -> UIDENT (L.Utf8.lexeme lexbuf)
  | '"' -> read_string (Buffer.create 32) lexbuf
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
  | _ ->
    let (s, _) = L.lexing_positions lexbuf in
    failwith (Printf.sprintf "Something went wrong at character %i" s.pos_cnum)

let makeSupplier = L.with_tokenizer token
