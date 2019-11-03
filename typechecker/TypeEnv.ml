open Base

type t = Scheme.t Map.M(String).t

let empty = Map.empty (module String)

let of_alist_exn : (string * Scheme.t) list -> t = Map.of_alist_exn (module String)

let extend (m: t) (k: String.t) (v: Scheme.t): t = Map.set m ~key:k ~data:v

let lookup (m: t) (k: String.t): Scheme.t Option.t = Map.find m k

let restrict (m: t) (k: String.t): t = Map.remove m k

let free_type_vars (env: t): Set.M(String).t =
  List.fold
    ~f:(fun set scheme -> Set.union set (Scheme.free_type_vars scheme))
    ~init:(Set.empty (module String))
    (Map.data env)
