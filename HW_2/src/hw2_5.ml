let rec filter f a l =
    match l with
    | [] -> []
    | hd::tl -> if (f hd a) then (filter f a tl) else hd::(filter f a tl)

let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
    match lst with
    | [] -> []
    | hd::tl -> hd::(filter (fun x y -> x == y) hd (uniq tl)) 