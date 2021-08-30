type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

exception TypeError

type typ = 
    TyUnit 
  | TyInt 
  | TyBool 
  | TyFun of typ * typ 
  | TyList of typ
  | TyVar of tyvar
and tyvar = string

(* List for EQUAL *)
let equal_list : typ list ref = ref []
let add_tyvar_to_equal_list =
  fun tyv ->
    equal_list := tyv :: (!equal_list)

(* You can invoke "fresh_tyvar()" to generate a fresh type variable *)
let tyvar_num = ref 0
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

(* ---------////////// Newly added //////////--------- *)

(* list function *)
let rec map f = function
  [] -> []
  | a::l -> let r = f a in r :: map f l
and my_list_assoc x = function
  [] -> raise Not_found
  | (a,b)::l -> if a = x then b else my_list_assoc x l

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = 
    fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = 
    fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = my_list_assoc x subst

  let rec apply : typ -> t -> typ 
  = fun typ subst ->
    match typ with
    | TyUnit -> TyUnit
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyFun (t1, t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyList x -> TyList (apply x subst)
    | TyVar x -> try find x subst with _ -> typ

  let extend tv ty subst = (tv, ty) :: (map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)
end

(* Type equation define *)
type typ_eqn = (typ * typ) list

(* Type equation generator *)
let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn
= fun tenv e ty -> (* TODO *)
  match e with
  | UNIT -> [(ty,TyUnit)]
  | TRUE -> [(ty,TyBool)] 
  | FALSE -> [(ty,TyBool)]
  | CONST _ -> [(ty, TyInt)]
  | VAR var -> [(ty, (TEnv.find tenv var))]
  | ADD (exp_1, exp_2) -> [(ty, TyInt)] @ (gen_equations tenv exp_1 TyInt) @ (gen_equations tenv exp_2 TyInt)
  | SUB (exp_1, exp_2) -> [(ty, TyInt)] @ (gen_equations tenv exp_1 TyInt) @ (gen_equations tenv exp_2 TyInt)
  | MUL (exp_1, exp_2) -> [(ty, TyInt)] @ (gen_equations tenv exp_1 TyInt) @ (gen_equations tenv exp_2 TyInt)
  | DIV (exp_1, exp_2) -> [(ty, TyInt)] @ (gen_equations tenv exp_1 TyInt) @ (gen_equations tenv exp_2 TyInt)
  | EQUAL (exp_1, exp_2) -> 
    let a_1 = fresh_tyvar () in
      (
        add_tyvar_to_equal_list a_1 ; [(ty, TyBool)] @ (gen_equations tenv exp_1 a_1) @ (gen_equations tenv exp_2 a_1)
      )       (* Overloading 처리 필요 *)
  | LESS (exp_1, exp_2) -> [(ty, TyBool)] @ (gen_equations tenv exp_1 TyInt) @ (gen_equations tenv exp_2 TyInt)
  | NOT exp -> [(ty, TyBool)]
  | NIL ->
    let a_1 = fresh_tyvar () in
      (
        [(ty, TyList a_1)]
      )
  | CONS (exp_1, exp_2) -> 
    let a_1 = fresh_tyvar () in 
      (
        [(ty, TyList a_1)] @ (gen_equations tenv exp_1 a_1) @ (gen_equations tenv exp_2 (TyList a_1))
      )      
  | APPEND (exp_1, exp_2) -> 
    let a_1 = fresh_tyvar () in 
      (
        [(ty, TyList a_1)] @ (gen_equations tenv exp_1 (TyList a_1)) @ (gen_equations tenv exp_2 (TyList a_1))
      )      
  | HEAD exp -> 
    let a_1 = fresh_tyvar () in 
      (
        [(ty, a_1)] @ (gen_equations tenv exp (TyList a_1))
      )
  | TAIL exp ->
    let a_1 = fresh_tyvar () in
      (
        [(ty, TyList a_1)] @ (gen_equations tenv exp (TyList a_1))
      )
  | ISNIL exp ->
    let a_1 = fresh_tyvar () in
      (
        [(ty, TyBool)] @ (gen_equations tenv exp (TyList a_1))
      )
  | IF (exp_1, exp_2, exp_3) -> (gen_equations tenv exp_1 TyBool) @ (gen_equations tenv exp_2 ty) @ (gen_equations tenv exp_3 ty)
  | LET (var, exp_1, exp_2) ->
    let a_1 = fresh_tyvar () in 
      (
        (gen_equations tenv exp_1 a_1) @ (gen_equations (TEnv.extend (var, a_1) tenv) exp_2 ty)
      )
  | LETREC (var_f, var_x, exp_1, exp_2) ->
    let a_1 = fresh_tyvar () in
    let a_2 = fresh_tyvar () in
    let tenv_new = TEnv.extend (var_f, TyFun(a_1, a_2)) tenv in
      (
        (gen_equations (TEnv.extend (var_x, a_1) tenv_new) exp_1 a_2) @ (gen_equations tenv_new exp_2 ty)
      )
  | LETMREC ( (var_f, var_x, exp_1), (var_g, var_y, exp_2), exp_3 ) ->
    let a_1 = fresh_tyvar () in
    let a_2 = fresh_tyvar () in
    let a_3 = fresh_tyvar () in
    let a_4 = fresh_tyvar () in
    let tenv_new = TEnv.extend (var_g, (TyFun (a_3, a_4))) 
                  (TEnv.extend (var_f, (TyFun (a_1, a_2))) tenv)   in
     (
      (gen_equations (TEnv.extend (var_x, a_1) tenv_new) exp_1 a_2) 
      @ (gen_equations (TEnv.extend (var_y, a_3) tenv_new) exp_2 a_4) 
      @ (gen_equations tenv_new exp_3 ty)
     )
  | PROC (var, exp) ->
    let a_1 = fresh_tyvar () in
    let a_2 = fresh_tyvar () in
      (
        [(ty, TyFun (a_1, a_2))] @ (gen_equations (TEnv.extend (var, a_1) tenv) exp a_2)
      )
  | CALL (exp_1, exp_2) ->
    let a_1 = fresh_tyvar () in
      (
        (gen_equations tenv exp_1 (TyFun (a_1, ty))) @ (gen_equations tenv exp_2 a_1)
      )
  | PRINT exp -> [(ty, TyUnit)]
  | SEQ (exp_1, exp_2) -> (gen_equations tenv exp_2 ty)

(* Type euqation solve *)
let rec unify : (typ * typ * Subst.t) -> Subst.t
= fun tup ->
    match tup with
    | (TyUnit, TyUnit, subst) -> subst
    | (TyInt, TyInt, subst) -> subst
    | (TyBool, TyBool, subst) -> subst
    | (TyList a, TyList b, subst) -> unify(a, b, subst)
    | (TyVar a, TyVar b, subst) -> if a = b then subst else Subst.extend a (TyVar b) subst
    | (TyVar a, t, subst) ->
      (
        match t with
        | TyFun (t_1, t_2) -> if (t_1 = TyVar a) || (t_2 = TyVar a) then raise (TypeError) else Subst.extend a t subst
        | _ -> Subst.extend a t subst
      )
    | (t, TyVar a, subst) -> unify(TyVar a, t, subst)
    | (TyFun (t_1, t_2), TyFun (t_1_p, t_2_p), subst) ->
      let s_p = unify (t_1, t_1_p, subst) in
      let s_pp = unify ((Subst.apply t_2 s_p), (Subst.apply t_2_p s_p), s_p) in
        (s_pp)
    | (_,_,_) -> raise (TypeError)

let rec unifyall : typ_eqn -> Subst.t -> Subst.t
= fun eqn subst ->
  match eqn with
  | [] -> subst
  | (t_1, t_2)::u -> let s_prime = unify ((Subst.apply t_1 subst), (Subst.apply t_2 subst), subst) in (unifyall u s_prime)

let solve : typ_eqn -> Subst.t 
= fun eqn ->
  unifyall eqn Subst.empty

(* EQUAL Check *)

exception EQUAL_error

let rec equal_check 
= fun equ_lst subst ->
  match equ_lst with
  | [] -> true
  | hd::tl -> 
  (
    match hd with
    | TyVar tyv ->
      (
        match Subst.find tyv subst with 
        | TyBool -> equal_check tl subst
        | TyInt -> equal_check tl subst
        | _ -> false
      )
    | _ -> raise (EQUAL_error)
  )

(* ---------////////// Added end //////////--------- *)

let typeof : exp -> typ 
=fun exp -> (* TODO *)
  tyvar_num := 0 ;
  equal_list := [] ;
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let subst = solve eqns in
  let ty = Subst.apply new_tv subst in
    if equal_check (!equal_list) subst then ty else raise(TypeError);;