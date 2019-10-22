open Base

let fail lexbuf _ =
  lexbuf 
  |> Sedlexing.lexeme_start
  |> Printf.sprintf "Syntax error at offset %d"
  |> Result.fail

let success e = Result.Ok e

let parse_expression lexbuf =
  let (first_pos, _) = Sedlexing.lexing_positions lexbuf in
  let checkpoint = Expression_parser.Incremental.parse_expression first_pos in
  let supplier = Lexer.makeSupplier lexbuf in
  Expression_parser.MenhirInterpreter.loop_handle
    success (fail lexbuf) supplier checkpoint

let parse_type_signature lexbuf =
  let (first_pos, _) = Sedlexing.lexing_positions lexbuf in
  let checkpoint = Expression_parser.Incremental.parse_type_signature first_pos in
  let supplier = Lexer.makeSupplier lexbuf in
  Expression_parser.MenhirInterpreter.loop_handle
    success (fail lexbuf) supplier checkpoint

let from_string = Sedlexing.Utf8.from_string
let test_expr s =
  from_string s
  |> parse_expression
  |> Result.map ~f:Ast.Syntax.expression_to_string
  |> Result.ok_or_failwith
  |> Stdio.print_endline

let%test_module "parse_expression" = (module struct
  let%expect_test "number constant" =
    test_expr "1";
    [%expect {| 1.000000 |}]

  let%expect_test "string constant" =
    test_expr "\"Test\"";
    [%expect {| Test |}]
  
  let%expect_test "identifier" =
    test_expr "test";
    [%expect {| test |}]
  
  let%expect_test "value binding" =
    test_expr "let x = 1 in x";
    [%expect {| let x = 1.000000 in x |}]

  let%expect_test "should parse a basic multiline program" =
    test_expr {|
    let f = fn x -> x in
    let y = 3 in 
    let x=4 in 
    f x+y
    f 3 + 4
    |};
    [%expect {| let f = fn x -> x in let y = 3.000000 in let x = 4.000000 in f x + y f 3.000000 + 4.000000 |}]

end)
