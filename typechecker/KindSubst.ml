open Base

type t = Kind.t Map.M(String).t

let null : t = Map.empty (module String)

let singleton s t : t = Map.singleton (module String) s t

let to_string (s: t) : string =
  Map.to_alist s
  |> List.map ~f:(fun (v, t) -> Printf.sprintf "%s = %s" v (Kind.to_string t))
  |> String.concat ~sep:", "

let rec kind_apply (subs: t) (k: Kind.t) : Kind.t =
  match k with
  | KindVar v -> Map.find subs v |> Option.value ~default:k
  | KindArrow (k1, k2) ->
      KindArrow (kind_apply subs k1, kind_apply subs k2)
  | KindType | KindRow -> k

let env_apply (subs: t) (env: KindEnv.t) : KindEnv.t =
    env |> Map.map ~f:(kind_apply subs)

let compose (s1: t) (s2: t) : t =
  Map.merge
    ~f:(fun ~key:_ x -> match x with `Left v | `Right v | `Both (_, v) -> Some v)
    (Map.map ~f:(kind_apply s1) s2)
    s1
