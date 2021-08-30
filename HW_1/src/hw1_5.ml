let isEven n =
    if n mod 2 = 0 then true else false;;

let square x =
    x * x;;

let rec fastexpt : int -> int -> int 
= fun b n ->
    if n = 1 then b
    else
        if (isEven n)
        then square (fastexpt b (n/2))
        else b * (fastexpt b (n-1));;