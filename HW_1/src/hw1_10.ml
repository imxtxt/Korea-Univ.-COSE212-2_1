type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval_exp : exp -> int
= fun e ->
  match e with
  | Num i -> i
  | Plus (exp1, exp2) -> eval_exp exp1 + eval_exp exp2
  | Minus (exp1, exp2) -> eval_exp exp1 - eval_exp exp2;;

let rec eval : formula -> bool
= fun f -> (* TODO *)
  match f with
  | True -> true
  | False -> false
  | Not f -> if (eval f) = true then false else true
  | AndAlso (f, g) -> 
    if (eval f) = false then false 
    else if (eval g) = true then true
    else false
  | OrElse (f, g) -> 
    if (eval f) = true then true
    else if (eval g) = true then true
    else false
  | Imply (f, g) -> eval (OrElse ((Not f), g))
  | Equal (exp1, exp2) -> if eval_exp exp1 = eval_exp exp2 then true else false;;