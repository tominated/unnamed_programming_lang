open Base
open Ast

let constant_to_js (constant: Syntax.constant): string =
  match constant with
  | Ast.Syntax.ConstNumber n -> Float.to_string n
  | Ast.Syntax.ConstString s -> s

let pattern_to_js ({ item ; _ }: Syntax.pattern): string =
  match item with
  | Ast.Syntax.PatternVar v -> v
  | _ -> failwith "Unsupported"

let rec expression_to_js ({ item ; _ }: Syntax.expression): string =
  match item with
  | Ast.Syntax.ExprUnit -> ""
  | Ast.Syntax.ExprConstant c -> constant_to_js c
  | Ast.Syntax.ExprIdent id -> id
  | Ast.Syntax.ExprValBinding (p, v, e) ->
      Printf.sprintf "(() => { const %s = %s; return %s; })()"
        (pattern_to_js p)
        (expression_to_js v)
        (expression_to_js e)
  | Ast.Syntax.ExprFn (arg, body) ->
      Printf.sprintf "(%s) => (%s)" arg (expression_to_js body)
  | Ast.Syntax.ExprApply (fn, arg) ->
      Printf.sprintf "(%s)(%s)"
        (expression_to_js fn)
        (expression_to_js arg)
  | Ast.Syntax.ExprInfix (lhs, op, rhs) ->
      Printf.sprintf "(%s) %s (%s)"
        (expression_to_js lhs)
        op
        (expression_to_js rhs)
  | Ast.Syntax.ExprTuple (xs) ->
        Printf.sprintf "[%s]" (String.concat ~sep:", " (xs |> List.map ~f:(fun expr -> expression_to_js expr)))
  | _ -> failwith "Unsupported"
