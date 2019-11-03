open Base

type t = Kind.t Map.M(String).t

let empty : t = Map.empty (module String)

let of_alist_exn : (string * Kind.t) list -> t = Map.of_alist_exn (module String)

let singleton (k: String.t) (v: Kind.t) : t = Map.singleton (module String) k v

let extend (m: t) (k: String.t) (v: Kind.t) : t = Map.set m ~key:k ~data:v

let lookup (m: t) (k: String.t) : Kind.t Option.t = Map.find m k

let restrict (m: t) (k: String.t) : t = Map.remove m k
