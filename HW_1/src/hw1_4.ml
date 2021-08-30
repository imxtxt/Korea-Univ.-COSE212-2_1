let rec dfact : int -> int
= fun n ->
    match n with
    | 1 | 2 -> n
    | _ -> n * dfac(n-2);;