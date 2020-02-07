open Base

module type S = sig
  include Monad.S

  type s
  val get : s t
  val put : s -> unit t
  val modify : (s -> s) -> unit t

  type 'a m
  val lift : 'a m -> 'a t

  val run_state_t : 'a t -> s -> ('a * s) m
end

module Make
  (S : T)
  (M : Monad.S) : S
  with type s = S.t
  and type 'a m = 'a M.t =
  struct
    module T = struct
      type 'a t = StateT of (S.t -> ('a * S.t) M.t)

      let un_state_t (StateT f) = f
      let run_state_t a s = (un_state_t a) s

      let return a = StateT (fun s -> M.return (a, s))
      let bind m ~f = StateT (fun s ->
        M.bind
          ((un_state_t m) s)
          ~f:(fun (a, s') -> (un_state_t (f a)) s')
      )
      let map = `Define_using_bind
    end

    include T
    include Monad.Make(T)

    type s = S.t
    let get = StateT (fun s -> M.return (s, s))
    let modify f = StateT (fun s -> M.return ((), f s))
    let put s = StateT (fun _ -> M.return ((), s))

    type 'a m = 'a M.t
    let lift m = StateT (fun s -> M.map m ~f:(fun a -> (a, s)))
  end
