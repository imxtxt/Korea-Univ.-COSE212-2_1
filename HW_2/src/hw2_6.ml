exception N_not_equal

let rec reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f l1 l2 accu ->
    match (l1, l2) with
    | ([], []) -> accu 
    | ([], _) -> raise N_not_equal
    | (_, []) -> raise N_not_equal
    | (hd_1::tl_1, hd_2::tl_2) -> reduce f tl_1 tl_2 (f hd_1 hd_2 accu);;