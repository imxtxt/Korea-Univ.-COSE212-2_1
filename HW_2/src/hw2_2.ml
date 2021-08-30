let rec forall : ('a -> bool) -> 'a list -> bool
= fun f lst -> (* TODO *)
    match lst with
    | [] -> true
    | hd::tl -> if (f hd) && (forall f tl) then true else false;;