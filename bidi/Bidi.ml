open Base

module TVar = struct
  module T = struct  
    type t = TVar of string
    let to_string (TVar s) = s
    let equal (TVar a) (TVar b) = String.equal a b
    let compare (TVar a) (TVar b) = String.compare a b
    let sexp_of_t (TVar s) : Sexp.t = Sexp.Atom s
  end

  include T
  include Comparable.Make(T)
end

module TEVar = struct
  module T = struct
    type t = TEVar of string
    let to_string (TEVar s) = Printf.sprintf "%s^" s
    let equal (TEVar a) (TEVar b) = String.equal a b
    let compare (TEVar a) (TEVar b) = String.compare a b
    let sexp_of_t (TEVar s) : Sexp.t = Sexp.Atom s
  end

  include T
  include Comparable.Make(T)
end

module EVar = struct
  module T = struct
    type t = EVar of string
    let to_string (EVar s) = s
    let equal (EVar a) (EVar b) = String.equal a b
    let compare (EVar a) (EVar b) = String.compare a b
    let sexp_of_t (EVar s) : Sexp.t =
      Sexp.List [Sexp.Atom "EVar"; Sexp.Atom s]
  end

  include T
  include Comparable.Make(T)
end

module TypeF = struct
  type 'a t =
    | Unit
    | Var of TVar.t
    | EVar of TEVar.t
    | Arrow of 'a * 'a
    | Forall of TVar.t * 'a
  
  let map ~f = function
    | Unit | Var _ | EVar _ as t -> t
    | Arrow (a, b) -> Arrow (f a, f b) 
    | Forall (v, t) -> Forall (v, f t)
end

module Type = struct
  include TypeF
  type t = t TypeF.t
  
  let rec to_string = function
    | Unit -> "Unit"
    | Var v -> TVar.to_string v
    | EVar v -> TEVar.to_string v
    | Arrow (a, b) -> Printf.sprintf "%s -> %s" (to_string a) (to_string b)
    | Forall (v, t) -> Printf.sprintf "∀%s.%s" (TVar.to_string v) (to_string t)
  
  let rec equal a b = match (a, b) with
    | Unit, Unit -> true
    | Var a, Var b -> TVar.equal a b
    | EVar a, EVar b -> TEVar.equal a b
    | Arrow (a1, a2), Arrow (b1, b2) -> equal a1 b1 && equal a2 b2
    | Forall (av, at), Forall (bv, bt) -> TVar.equal av bv && equal at bt
    | _ -> false

  let rec is_mono = function
    | Arrow (a, b) -> is_mono a && is_mono b
    | Forall _ -> false
    | _ -> true
  
  let rec free_vars = function
    | Unit | Var _ -> Set.empty (module TEVar)
    | EVar v -> Set.singleton (module TEVar) v
    | Arrow (a, b) -> Set.union (free_vars a) (free_vars b)
    | Forall (_, t) -> free_vars t
  
  let rec instantiate (v, s) t = match ((v, s), t) with
    | _, Unit -> Unit
    | (v, s), Var v' when TVar.equal v v' -> s
    | _, (Var _ as t) -> t
    | _, (EVar _ as t) -> t 
    | s, Arrow (a, b) -> Arrow (instantiate s a, instantiate s b)
    | s, Forall (v, t) -> Forall (v, instantiate s t)
end

module ExprF = struct
  type 'a t =
    | Unit
    | Var of EVar.t
    | Fn of { arg: string; body: 'a }
    | Apply of { fn: 'a; arg: 'a }
  
  let map ~f = function
    | Unit | Var _ as t -> t
    | Fn { arg; body } -> Fn { arg; body = f body }
    | Apply { fn; arg } -> Apply { fn = f fn; arg = f arg }
end

module Expr = struct
  type t = t ExprF.t
  let map: f:(t -> t) -> t -> t = ExprF.map
end

module ContextMember = struct
  type t =
    | Var of TVar.t
    | Assump of EVar.t * Type.t
    | EVar of TEVar.t
    | Solved of TEVar.t * Type.t
    | Marker of TEVar.t
  
  let equal a b = match (a, b) with
    | Var a, Var b -> TVar.equal a b
    | Assump (av, at), Assump (bv, bt) -> EVar.equal av bv && Type.equal at bt
    | EVar a, EVar b -> TEVar.equal a b
    | Solved (av, at), Solved (bv, bt) -> TEVar.equal av bv && Type.equal at bt
    | Marker a, Marker b -> TEVar.equal a b
    | _ -> false
end

module Context = struct
  type t = Context of ContextMember.t List.t

  let default : t = Context []

  let append_end x (Context ctx) : t =
    Context (List.append ctx [x])
  
  let mem x (Context ctx) =
    List.mem ~equal:ContextMember.equal ctx x
  
  let hole (x: ContextMember.t) (Context ctx) : (t * t) Option.t =
    if List.mem ~equal:ContextMember.equal ctx x
    then
      let (a, b) = List.split_while ctx ~f:(ContextMember.equal x) in
      Some (Context a, Context (List.drop b 1))
    else None
  
  let hole2 (a: ContextMember.t) (b: ContextMember.t) (ctx: t) : (t * t * t) Option.t =
    Option.bind (hole a ctx) ~f:(fun (a, ctx') ->
      Option.bind (hole b ctx') ~f:(fun (b, c) ->
        Some (a, b, c)
      )
    )
  
  exception MultipleTypesForVariable 

  let assump (Context ctx) (x: EVar.t) : Type.t Option.t =
    let assumptions = List.filter ctx ~f:(function
      | Assump (y, _) -> EVar.equal x y
      | _ -> false
    ) in
    match assumptions with 
    | [Assump (_, t)] -> Some t
    | [] -> None
    | _ -> raise MultipleTypesForVariable
  
  let solution (Context ctx) (v: TEVar.t) : Type.t Option.t =
    let solutions = List.filter ctx ~f:(function
      | Solved (u, _) -> TEVar.equal v u
      | _ -> false
    ) in
    match solutions with 
    | [Solved (_, t)] -> Some t
    | [] -> None
    | _ -> raise MultipleTypesForVariable
  
  let until m (Context ctx) =
    let members = List.take_while ctx ~f:(fun m' -> ContextMember.equal m m' |> not) in
    Context members
  
  let rec apply_subst (ctx: t) : Type.t -> Type.t = function
    | Unit -> Unit
    | Var _ as t -> t
    | EVar v as t -> t (* TODO: add impl *)
    | Arrow (a, b) -> Arrow (apply_subst ctx a, apply_subst ctx b)
    | Forall (v, t) -> Forall (v, apply_subst ctx t)
end

module CheckResult = struct
  module T = struct
    type error = | Err of string
    type 'a t = ('a, error) Result.t

    let map = `Define_using_bind
    let return = Result.return
    let bind = Result.bind
  end

  include T
  include Monad.Make(T)
end

module CheckState = struct
  type t = { ctx : Context.t; nextEVar : Int.t }
  let default : t = { ctx = Context.default; nextEVar = 1 }
end

module CheckM = struct
  include StateT.Make (CheckState) (CheckResult)

  exception Unimplemented

  let (let*) m f = bind m ~f
  let (let+) m f = map m ~f
  let pure = return

  let throw e = lift (Error e)

  let run_check_m m =
    run_state_t m CheckState.default
  
  let modify_ctx f = modify (fun { ctx; nextEVar } -> { ctx = f ctx; nextEVar })

  let fresh_evar : TEVar.t t =
    let* { nextEVar } = get in
    let* _ = modify (fun { ctx;_ } -> { ctx; nextEVar = nextEVar + 1 }) in
    let id = Printf.sprintf "a%i" nextEVar in
    return (TEVar.TEVar id)
  
  let rec ty_sub (a: Type.t) (b: Type.t) : unit t =
    match (a, b) with
    | Unit, Unit -> return ()
    | Var a, Var b when TVar.equal a b -> return ()
    | EVar a, EVar b when TEVar.equal a b -> return ()
    | Arrow (a, b), Arrow (a', b') ->
        let* _ = ty_sub a' a in
        ty_sub b b'
    | Forall (v, a), b ->
        let* a' = fresh_evar in
        let a'' = Type.instantiate (v, EVar a') a in
        let* _ = modify_ctx (fun c ->
          c |> Context.append_end (Marker a') |> Context.append_end (EVar a')
        ) in
        let* _ = ty_sub a'' b in
        modify_ctx (Context.until (Marker a'))
    | a, Forall (v, b) ->
        let* _ = modify_ctx (Context.append_end (Var v)) in
        let* _ = ty_sub a b in
        modify_ctx (Context.until (Var v))
    | EVar a', a when not (Set.mem (Type.free_vars a) a') -> raise Unimplemented
    | a, EVar a' when not (Set.mem (Type.free_vars a) a') -> raise Unimplemented
    | _ -> raise Unimplemented
end
