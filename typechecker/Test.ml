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

module Infer = struct
  type error = [
    | `UnboundVariable of string
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

  let unify (t1: Type.t) (t2: Type.t) = tell [(t1, t2)]

  let inEnv id scheme m =
    local (fun e -> TypeEnv.extend e id scheme) m

  let fresh =
    let open Let_syntax in
    let%bind x = get in
    let%bind _ = put (x + 1) in
    return (TypeVar (Printf.sprintf "t%d" x) |> locate)

  let except (e: error) = lift (Error e)

  let instantiate (Forall (vars, t)) =
    let open Let_syntax in
    let%bind new_vars = List.map ~f:(fun _ -> fresh) vars |> all in
    let subst = TypeSubst.of_alist_exn (List.zip_exn vars new_vars) in
    return (TypeSubst.type_apply subst t)


  let lookup id =
    let open Let_syntax in
    let%bind env = ask in
    match TypeEnv.lookup env id with
    | None -> except (`UnboundVariable id)
    | Some scheme -> instantiate scheme

  let rec infer (e: expression) : Type.t t =
    let open Let_syntax in
    match e.item with
    | ExprUnit -> return (TypeUnit |> TypeInfer.locate)

    | ExprIdent id -> lookup id

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

    | ExprRecordEmpty -> return (TypeRecord (TypeInfer.locate TypeRowEmpty) |> TypeInfer.locate)

    | ExprRecordExtend (label, expr, rest) ->
        let%bind field_t = fresh in
        let%bind rest_t = fresh in
        let record_rest_t = TypeRecord rest_t |> TypeInfer.locate in
        let%bind expr_t = infer expr in
        let%bind rest_t = infer rest in
        let%bind _ = unify field_t expr_t in
        let%bind _ = unify record_rest_t rest_t in
        TypeRecord (TypeRowExtend (label, field_t, rest_t) |> TypeInfer.locate) |> TypeInfer.locate
        |> return

    | _ -> lift (Error (`Unimplemented "nice"))
end