let addf : (int -> int) -> (int -> int) -> int -> int
= fun f g x ->
    f(g x);;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) -> (* TODO *)
    if n = 1 then f
    else addf f (iter ((n-1), f));;