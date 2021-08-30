type btree = 
  | Leaf of int
  | Left of btree
  | Right of btree
  | LeftRight of btree * btree

let rec mirror : btree -> btree
= fun tree -> (*TODO*)
    match tree with
    | Leaf x -> Leaf x
    | Left x -> Right (mirror x)
    | Right x -> Left (mirror x) 
    | LeftRight (x, y) -> LeftRight (y, x);;