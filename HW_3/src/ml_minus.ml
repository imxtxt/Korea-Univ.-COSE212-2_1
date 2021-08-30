type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of (var * var * exp) * (var * var * exp) * env (* TODO *)
and env = (var * value) list

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

exception UndefinedSemantics

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  (* TODO *) 
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR x -> lookup_env x env
  | ADD (e1, e2) -> 
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (
        match v1,v2 with
        | Int n1, Int n2 -> Int (n1 + n2)
        | _ -> raise (UndefinedSemantics)
      )
  | SUB (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (
        match v1,v2 with
        | Int n1, Int n2 -> Int (n1 - n2)
        | _ -> raise (UndefinedSemantics)
      )
  | MUL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (
        match v1,v2 with
        | Int n1, Int n2 -> Int (n1 * n2)
        | _ -> raise (UndefinedSemantics)
      )
  | DIV (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (
        match v1, v2 with
        | Int n1, Int n2 ->
          (
            match n2 with
            | 0 -> raise (UndefinedSemantics)
            | _ -> Int (n1 / n2)
          )
        | _ -> raise (UndefinedSemantics)
      )
  | EQUAL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (
        match v1, v2 with
        | Int n1, Int n2 ->
            if n1 = n2 then Bool true else Bool false
        | Bool b1, Bool b2 ->
            if b1 = b2 then Bool true else Bool false
        | _ -> raise (UndefinedSemantics)
      )
  | LESS (e1, e2) -> 
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (
        match v1, v2 with
        | Int n1, Int n2 ->
            if n1 < n2 then Bool true else Bool false
        | _ -> raise (UndefinedSemantics)
      )
  | NOT exp ->
    let v1 = eval exp env in
    (
      match v1 with
      | Bool true -> Bool false
      | Bool false -> Bool true
      | _ -> raise (UndefinedSemantics)
    )
  | NIL -> List []
  | CONS (e1, e2) ->            
    let v = eval e1 env in
    let s = eval e2 env in
      (
        match v, s with
        | _, List lst ->
          (
            match lst with
            | _ -> List (v::lst)
          )
        | _ -> raise (UndefinedSemantics)
      )
  | APPEND (e1, e2) ->         
    let s1 = eval e1 env in
    let s2 = eval e2 env in
      (
        match s1, s2 with
        | List lst1, List lst2 ->
          (
            match lst1, lst2 with
            | _ -> List (lst1@lst2)
          )
        | _ -> raise (UndefinedSemantics)
      )
  | HEAD exp ->
    let v = eval exp env in
    (
      match v with
      | List lst ->
        (
          match lst with
          | [] -> List []
          | hd::_ -> hd 
        )
      | _ -> raise (UndefinedSemantics)
    )
  | TAIL exp ->
    let v = eval exp env in
    (
      match v with
      | List lst ->
        (
          match lst with
          | [] -> List []
          | _::tl -> List tl
        )
      | _ -> raise (UndefinedSemantics)
    )
  | ISNIL exp ->
    let v = eval exp env in
    (
      match v with
      | List lst ->
        (
          match lst with
          | hd::tl -> Bool false
          | _ -> Bool true
        )
      | _ -> raise (UndefinedSemantics)
    )
  | IF (e1, e2, e3) ->
    let v1 = eval e1 env in
    (
      match v1 with
      | Bool true -> (eval e2 env) 
      | Bool false -> (eval e3 env) 
      | _ -> raise (UndefinedSemantics)
    )
  | LET (var1, e1, e2) ->
    let v1 = eval e1 env in
    let env_new = extend_env (var1, v1) env in
    (
      eval e2 env_new
    )
  | LETREC (var_f, var_x, e1, e2) ->
    let tuple_proc = RecProcedure (var_f, var_x, e1, env) in
    let env_new = extend_env (var_f,tuple_proc) env in
    (
      eval e2 env_new
    )
  | LETMREC ((var_f, var_x, e1), (var_g, var_y, e2), e3) ->
    let tup_mproc1 = MRecProcedure ((var_f, var_x, e1), (var_g, var_y, e2), env) in
    let tup_mproc2 = MRecProcedure ((var_g, var_y, e2),(var_f, var_x, e1), env) in
    let env_new = extend_env (var_f, tup_mproc1) env in
    let env_new = extend_env (var_g, tup_mproc2) env_new in
    (
      eval e3 env_new
    ) 
  | PROC (var1, e1) ->
    Procedure (var1, e1, env)
  | CALL (e1, e2) ->
  let v2 = eval e2 env in
  let v1 = eval e1 env in
  (
    match v1 with
    | Procedure (var_x, e, env_p) ->
    (
      let env_new = extend_env (var_x, v2) env_p in
      (
        eval e env_new
      )
    )
    | RecProcedure (var_f, var_x, e, env_p) ->
    (
      let env_new = extend_env (var_x, v2) env_p in
      let env_new = extend_env (var_f, RecProcedure (var_f, var_x, e, env_p)) env_new in
      (
        eval e env_new
      )
    )
    | MRecProcedure ((var_f, var_x, e_f), (var_g, var_y, e_g), env_p) ->
    (
      let env_new = extend_env (var_x, v2) env_p in
      let env_new = extend_env (var_f, MRecProcedure ((var_f, var_x, e_f), (var_g, var_y, e_g), env_p)) env_new in
      let env_new = extend_env (var_g, MRecProcedure ((var_g, var_y, e_g), (var_f, var_x, e_f), env_p)) env_new in
      (
        eval e_f env_new
      )
    )
    | _ -> raise (UndefinedSemantics)
  )
  | SEQ (e1, e2) ->
    let v1 = eval e1 env in
      (
        eval e2 env
      )

let runml : program -> value
=fun pgm -> eval pgm empty_env;;
