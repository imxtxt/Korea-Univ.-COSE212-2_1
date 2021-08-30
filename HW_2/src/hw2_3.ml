let double : ('a -> 'a) -> 'a -> 'a
= fun f -> (* TODO *)
    fun x -> f (f x);;