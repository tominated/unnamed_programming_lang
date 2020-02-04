open Base

module type MONOID = sig
  type t
  val empty : t
  val append : t -> t -> t
end

module type RWST = sig
  include Monad.S

  type r
  val ask : r t
  val local : (r -> r) -> 'a t -> 'a t

  type w
  val tell : w -> unit t

  type s
  val get : s t
  val put : s -> unit t
  val modify : (s -> s) -> unit t

  type 'a m
  val lift : 'a m -> 'a t

  val runRWST : 'a t -> r -> s -> ('a * s * w) m
end

module Make
  (R : sig type t end)
  (W : MONOID)
  (S : sig type t end)
  (M : Monad.S) :
  RWST
  with type r = R.t
  and type w = W.t
  and type s = S.t
  and type 'a m = 'a M.t =
  struct
    type 'a t_ = RWST of (R.t -> S.t -> ('a * S.t * W.t) M.t)
    type 'a t = 'a t_
    let unRWST (RWST f) = f

    let runRWST a r s = (unRWST a) r s

    (* Monad *)
    let return a = RWST (fun _ s -> M.return (a, s, W.empty))
    let bind m ~f:k = RWST (fun r s ->
      let open M.Let_syntax in
      let f = unRWST m in
      let%bind (a, s', w) = f r s in
      let f' = unRWST (k a) in
      let%bind (b, s'', w') = f' r s' in
      return (b, s'', W.append w w')
    )

    include Monad.Make(struct
      type 'a t = 'a t_
      let bind = bind
      let return = return
      let map = `Define_using_bind
    end)

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
