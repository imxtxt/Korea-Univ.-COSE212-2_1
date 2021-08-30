type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

exception Empty

let rec diff : aexp * string -> aexp
= fun (exp, x) -> (* TODO *)
    match exp with
    | Sum [] -> raise Empty
    | Sum ([hd]) -> diff (hd, x) 
    | Sum (hd::tl) -> Sum ((diff (hd, x)) :: diff ((Sum tl), x) :: [])
    
    | Times [] -> raise Empty
    | Times ([a]) -> diff (a, x)
    | Times (hd::tl) ->
        Sum [Times ([(diff (hd, x))]@tl); Times ([hd]@[(diff ((Times tl), x))])]

    | Const _ -> Const 0

    | Var i -> 
        if i = x then Const 1 else Const 0 

    | Power (i, n) -> 
    if i = x then
        Times [Const n; Power(i, (n-1))]
    else Const 0;;