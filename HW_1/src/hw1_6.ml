let rec binarize : int -> int list
= fun n ->
    if n = 0 then [0]
    else if n = 1 then [1]
    else (binarize (n / 2)@([n mod 2]));;