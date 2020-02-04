open Base
open Ast.Syntax

module type MONOID = sig
  type t
  val empty : t
  val append : t -> t -> t
end

module ListM = functor(A: sig type t end) -> (
  struct
    type t = A.t List.t
    let empty = []
    let append a b = a @ b
  end : MONOID with type t = A.t List.t
)

module type RWS = sig
  include Monad.Basic

  type r
  val ask : r t
  val local : (r -> r) -> 'a t -> 'a t

  type w
  val tell : w -> unit t

  type s
  val get : s t
  val put : s -> unit t
  val modify : (s -> s) -> unit t
end

module type RWST = sig
  include RWS

  type 'a m
  val lift : 'a m -> 'a t

  val runRWST : 'a t -> r -> s -> ('a * s * w) m
end

module RWST
  (R : sig type t end)
  (W : MONOID)
  (S : sig type t end)
  (M : Monad.S) :
  RWST with type r = R.t
  and type w = W.t
  and type s = S.t
  and type 'a m = 'a M.t = struct
    type 'a t = RWST of (R.t -> S.t -> ('a * S.t * W.t) M.t)
    let unRWST (RWST f) = f

    let runRWST a r s = (unRWST a) r s

    (* Monad *)
    let map = `Define_using_bind
    let return a = RWST (fun _ s -> M.return (a, s, W.empty))
    let bind m ~f:k = RWST (fun r s ->
      let open M.Let_syntax in
      let f = unRWST m in
      let%bind (a, s', w) = f r s in
      let f' = unRWST (k a) in
      let%bind (b, s'', w') = f' r s' in
      return (b, s'', W.append w w')
    )

    (* MonadReader *)
    type r = R.t
    let ask = RWST (fun r s -> M.return (r, s, W.empty))
    let local f m = RWST (fun r s -> runRWST m (f r) s)

    (* MonadWriter *)
    type w = W.t
    let tell w = RWST (fun _ s -> M.return ((), s, w))

    (* MonadState *)
    type s = S.t
    let get = RWST (fun _ s -> M.return (s, s, W.empty))
    let modify f = RWST (fun _ s -> M.return ((), (f s), W.empty))
    let put s = RWST (fun _ _ -> M.return ((), s, W.empty))

    (* MonadTrans *)
    type 'a m = 'a M.t
    let lift m = RWST (fun _ s ->
      let open M.Let_syntax in
      let%bind a = m in
      return (a, s, W.empty)
    )
  end

module Infer = struct
  type error = [
    | `UndefinedIdentifier of string
    | `Unimplemented of string
  ]

  module R = struct
    type t = TypeEnv.t
  end

  module W = struct
    include ListM (struct
      type t = Type.t * Type.t
    end)

    let singleton (x: Type.t * Type.t) : t = [x]
  end

  module S = struct
    type t = int
  end

  module InferResult = struct
    module T = struct
      type 'a t = ('a, error) Result.t
      let bind x ~f = Result.bind ~f x
      let return x = Ok x
      let map = `Custom Result.map
    end

    type 'a t = 'a T.t

    include Monad.Make(T)
  end

  module M = RWST (R) (W) (S) (InferResult)
  include M

  module Let_syntax = struct
    let return = return
    let bind = bind
  end

  let unify (t1: Type.t) (t2: Type.t) = tell [(t1, t2)]

  let inEnv id scheme m =
    local (fun e -> TypeEnv.extend e id scheme) m

  let fresh =
    get |>
      bind ~f:(fun x ->
        put (x + 1)
        |> bind ~f:(fun _ ->
          return (TypeVar (Printf.sprintf "t%d" x) |> locate)
        )
      )

  let except (e: error) = lift (Error e)

  let rec infer (e: expression) : Type.t t =
    match e.item with
    | ExprUnit -> return (TypeUnit |> TypeInfer.locate)

    | ExprFn (arg, body) ->
        let%bind tv = fresh in
        let%bind t = inEnv arg (Scheme.from_type tv) (infer body) in
        return (TypeArrow (tv, t) |> TypeInfer.locate)

    | ExprApply (fn, arg) ->
        let%bind fn_type = infer fn in
        let%bind arg_type = infer arg in
        let%bind rt_type = fresh in
        let%bind _ = unify fn_type (TypeArrow (arg_type, rt_type) |> TypeInfer.locate) in
        return rt_type

    | ExprValBinding (pattern, value, body) -> begin
      match pattern.item with
      | PatternVar id -> begin
        let%bind env = ask in
        let%bind value_type = infer value in
        let scheme = TypeInfer.generalise env value_type in
        let%bind body_type = inEnv id scheme (infer body) in
        return body_type
      end
      | _ -> except (`Unimplemented "other patterns")
    end

    | _ -> lift (Error (`Unimplemented "nice"))
end
