open Base

module type S = sig
  include Monad.S

  type s
  val get : s t
  val put : s -> unit t
  val modify : (s -> s) -> unit t

  type 'a m
  val lift : 'a m -> 'a t

  val runStateT : 'a t -> s -> ('a * s) m
end

module Make
  (S : sig type t end)
  (M : Monad.S) : S
  with type s = S.t
  and type 'a m = 'a M.t =
  struct
    type 'a t_ = StateT of (S.t -> ('a * S.t) M.t)
    type 'a t = 'a t_

    let unStateT (StateT f) = f
    let runStateT a s = (unStateT a) s

    let return a = StateT (fun s -> M.return (a, s))
    let bind m ~f = StateT (fun s ->
      let open M.Let_syntax in
      let%bind (a, s') = (unStateT m) s in
      let%bind (b, s'') = (unStateT (f a)) s' in
      return (b, s'')
    )

    include Monad.Make(struct
      type 'a t = 'a t_
      let bind = bind
      let return = return
      let map = `Define_using_bind
    end)

    type s = S.t
    let get = StateT (fun s -> M.return (s, s))
    let modify f = StateT (fun s -> M.return ((), f s))
    let put s = StateT (fun _ -> M.return ((), s))

    type 'a m = 'a M.t
    let lift m = StateT (fun s ->
      let open M.Let_syntax in
      let%bind a = m in
      return (a, s)
    )
  end
