let rec zipper : int list * int list -> int list
= fun (l1, l2) -> (* TODO *) 
    match (l1, l2) with
    | ([], []) -> []
    | ([], b) -> b
    | (a, []) -> a
    | (hd_1::tl_1, hd_2::tl_2) -> [hd_1;hd_2]@(zipper (tl_1,tl_2));; 