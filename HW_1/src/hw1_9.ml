type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n t -> (* TODO *)
	match t with
	| Empty -> false
	| Node (x, lt, rt) -> 
		if x = n then true 
		else if mem n lt then true
		else if mem n rt then true
		else false;;