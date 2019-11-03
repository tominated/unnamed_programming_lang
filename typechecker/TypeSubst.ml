open Base
open Ast.Syntax

type t = Type.t Map.M(String).t

let null : t = Map.empty (module String)

let singleton s t : t = Map.singleton (module String) s t

let to_string (s: t) : string =
  Map.to_alist s
  |> List.map ~f:(fun (v, t) -> Printf.sprintf "%s = %s" v (Type.to_string t))
  |> String.concat ~sep:", "

let rec type_apply (subst: t) (t: Type.t) : Type.t =
  match t.item with
  | TypeVar v ->
    Map.find subst v |> Option.value ~default:t
  | TypeArrow (t1, t2) -> {
      item = TypeArrow (type_apply subst t1, type_apply subst t2);
      location = t.location
    }
  | TypeTuple ts -> {
      item = TypeTuple (List.map ~f:(type_apply subst) ts);
      location = t.location;
    }
  | TypeConstructor (n, ts) -> {
      item = TypeConstructor (n, List.map ~f:(type_apply subst) ts);
      location = t.location;
    }
  | _ -> t

let scheme_apply (subs: t) (scheme: Scheme.t) : Scheme.t =
  match scheme with
  | Forall (vars, t) ->
    let subs_ = vars |> List.fold ~init:subs ~f:Map.remove in
    Forall (vars, type_apply subs_ t)

let env_apply (subs: t) (env: TypeEnv.t) : TypeEnv.t =
    env |> Map.map ~f:(scheme_apply subs)

let compose (s1: t) (s2: t) : t =
  Map.merge
    ~f:(fun ~key:_ x -> match x with `Left v | `Right v | `Both (_, v) -> Some v)
    (Map.map ~f:(type_apply s1) s2)
    s1
