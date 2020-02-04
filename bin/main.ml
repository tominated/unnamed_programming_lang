open Base
open Parser
open Ast
open Typechecker

let kind_env = KindEnv.of_alist_exn [
  ("Number", KindType);
  ("Boolean", KindType);
  ("String", KindType);
  ("List", KindArrow(KindType, KindType));
  ("Option", KindArrow(KindType, KindType));
  ("Result", KindArrow(KindType, KindArrow(KindType, KindType)));
]

let type_env =
  let vars = [
    ("id", "forall a. a -> a");
    ("add", "forall . Number -> Number -> Number");
    ("+", "forall . Number -> Number -> Number");
    ("-", "forall . Number -> Number -> Number");
    ("==", "forall . Number -> Number -> Boolean");
    ("succ", "forall . Number -> Number");
    ("true", "forall . Boolean");
    ("false", "forall . Boolean");
    ("not", "forall . Boolean -> Boolean");
    ("some", "forall a. a -> Option a");
    ("none", "forall . Option a");
    ("bind", "forall a b. (Option a) -> (a -> Option b) -> Option b");
    (">>=", "forall a b. (Option a) -> (a -> Option b) -> Option b");
  ] in
  let parse_and_add env (id, body) =
    let buf = Sedlexing.Utf8.from_string body in
    match Parser.parse_scheme buf with
    | Ok scheme -> TypeEnv.extend env id scheme
    | Error e -> failwith (Printf.sprintf "%s: %s" id e)
  in
  List.fold ~init:TypeEnv.empty ~f:parse_and_add vars

let parse lexbuf =
  match (Parser.parse_expression lexbuf) with
  | Ok e -> begin
    match Test.run type_env e with
    | Ok t ->
      let expr_str = Syntax.expression_to_string e in
      let type_str = Syntax.type_signature_to_string t in
      Stdio.printf "    %s\n    %s\n" expr_str type_str
    | Error err -> Stdio.print_endline (Test.Error.to_string err)
    end
  | Error msg -> msg |> Stdio.print_endline

let rec loop () =
  Stdio.printf "Enter Exp: %!";
  match Stdio.In_channel.input_line Stdio.stdin with
  | None | Some "exit" | Some "quit" -> ()
  | Some s ->
    parse (Sedlexing.Utf8.from_string s);
    loop ()

let () = loop ()
