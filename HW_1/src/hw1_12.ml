let rec drop : ('a -> bool) -> 'a list -> 'a list
= fun f lst -> (* TODO *)
    match lst with
    | [] -> []
    | hd::tl -> if (f hd) = false then lst else (drop f tl);;