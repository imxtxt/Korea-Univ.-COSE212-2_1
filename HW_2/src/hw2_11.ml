type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> (* TODO *)
    match exp with
    | INT a -> a
    | ADD (x, y) -> (calculator x) + (calculator y)
    | SUB (x, y) -> (calculator x) - (calculator y)
    | MUL (x, y) -> (calculator x) * (calculator y)
    | DIV (x, y) -> (calculator x) / (calculator y)
    | SIGMA (a, b, f) -> 
    if (calculator a) = (calculator b) then (calculator_env (calculator b) f) 
    else if a < b then
      calculator (SIGMA (a, INT ((calculator b)-1), f)) + (calculator_env (calculator b) f)
    else 0
    | _ -> raise (Failure ("Error : INVAILD ENV"))
and calculator_env : int -> exp -> int
= fun env exp ->
  match exp with
  | X -> env
  | INT a -> a
  | ADD(x, y) -> 
  (calculator_env env x) + (calculator_env env y)
  | SUB(x, y) -> 
  (calculator_env env x) - (calculator_env env y)
  | MUL(x, y) -> 
  (calculator_env env x) * (calculator_env env y)
  | DIV(x, y) -> 
  (calculator_env env x) / (calculator_env env y)
  | SIGMA _ -> calculator exp;;