type digit = ZERO | ONE
type bin = digit list

exception Error

let rec mylength : 'a list -> int
= fun n ->
    match n with
    | [] -> 0
    | hd::tl -> 1 + (mylength tl)

let rec power_of_2
= fun n ->
    match n with
    | 0 -> 1
    | _ -> 2 * power_of_2 (n-1)

let num_to_digit
= fun n ->
    match n with
    | 0 -> ZERO
    | 1 -> ONE
    | _ -> raise Error

let rec binarize
= fun n ->
    match n with
    | 0 -> [ZERO]
    | 1 -> [ONE]
    | _ -> (binarize (n / 2))@[num_to_digit (n mod 2)]

let rec decimalize : bin -> int
= fun n ->
    match n with
    | [] -> raise Error
    | [ZERO] -> 0
    | [ONE] -> 1
    | ZERO::tl -> decimalize tl
    | ONE::tl -> (power_of_2 ((mylength n) - 1)) + (decimalize tl)

let bmul : bin -> bin -> bin
= fun a b -> (* TODO *)
    binarize ((decimalize a) * (decimalize b));;