let rec prime_internal a b =
    if b = a then true
    else 
        if a mod b = 0 then false else prime_internal a (b+1);;

let prime : int -> bool
= fun n ->
      match n with
    | 0 | 1 -> false
    | _ -> (prime_internal n 2);;