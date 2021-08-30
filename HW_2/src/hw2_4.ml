let rec dup_check_pass : 'a -> 'a list -> bool
= fun a lst ->
    match lst with
    | [] -> true
    | hd::tl -> if (a != hd) && (dup_check_pass a tl) then true else false;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
    let l2_uniq = duple_delete l2 in
    match l1 with
    | [] -> l2_uniq 
    | hd::tl -> if (dup_check_pass hd l2_uniq) then (app tl (l2_uniq@[hd])) else (app tl l2_uniq) 
and duple_delete
= fun lst ->
    match lst with
    | [] -> lst
    | hd::tl -> if (dup_check_pass hd (duple_delete tl)) then hd::(duple_delete tl) else (duple_delete tl);;