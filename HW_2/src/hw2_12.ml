type exp = 
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string

let isempty lst =
  match lst with
  | [] -> true
  | _ -> false

let rec filter f a l =
    match l with
    | [] -> []
    | hd::tl -> if (f hd a) then (filter f a tl) else hd::(filter f a tl)

let rec list_sub lst1 lst2 =
  match lst1 with
  | [] -> []
  | l_1 ->
    match lst2 with
    | [] -> l_1
    | hd_2::tl_2 -> list_sub (filter (fun x y -> x = y) hd_2 l_1) tl_2  

let rec getFV
= fun exp ->
  match exp with
  | V x -> [x]
  | P (x, e) -> getFV e
  | C (e_1, e_2) -> (getFV e_1)@(getFV e_2) 

let rec getBV
= fun exp ->
  match exp with
  | V x -> [] 
  | P (x, e) -> [x] @ getBV e
  | C (e_1, e_2) -> (getBV e_1)@(getBV e_2)

let check : exp -> bool
= fun exp -> (* TODO *)
  if isempty (list_sub (getFV exp) (getBV exp)) then true else false;;