type nat = ZERO | SUCC of nat;;

let rec intToNat : int -> nat
= fun n ->
    if n = 0 then ZERO
    else SUCC (intToNat (n-1));;

let rec natToInt x = 
    match x with
    | ZERO -> 0
    | SUCC x' -> 1 + (natToInt x');;

let natadd : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
    intToNat ((natToInt n1) + (natToInt n2));;

let natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
    intToNat ((natToInt n1) * (natToInt n2));;