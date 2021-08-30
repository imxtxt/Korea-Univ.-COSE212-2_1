type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp                 (* sequence *)
  | IF of exp * exp * exp            (* if-then-else *)
  | WHILE of exp * exp               (* while loop *)
  | LETV of id * exp * exp           (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list           (* call by value *)
  | CALLR of id * id list            (* call by referenece *)
  | RECORD of (id * exp) list        (* record construction *)
  | FIELD of exp * id                (* access record field *)
  | ASSIGN of id * exp               (* assgin to variable *)
  | ASSIGNF of exp * id * exp        (* assign to record field *)
  | WRITE of exp
and id = string

type loc = int
type value =
| Num of int
| Bool of bool
| Unit
| Record of record 
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

(********************************)
(*     Handling environment     *)
(********************************)

let rec lookup_loc_env : id -> env -> loc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind (id,l) -> if(x=id) then l else lookup_loc_env x tl
    | ProcBind _ -> lookup_loc_env x tl
    end

let rec lookup_proc_env : id -> env -> proc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind _ -> lookup_proc_env x tl
    | ProcBind (id,binding) -> if (x=id) then binding else lookup_proc_env x tl
    end

let extend_env : binding -> env -> env
= fun e env -> e::env

let empty_env = []


(***************************)
(*     Handling memory     *)
(***************************)

let rec lookup_mem : loc -> memory -> value
= fun l mem ->
  match mem with
  | [] -> raise(Failure ("location "^(string_of_int l)^" is not included in memory"))
  | (loc,v)::tl -> if(l=loc) then v else lookup_mem l tl

let extend_mem : (loc * value) -> memory -> memory
= fun (l,v) mem -> (l,v)::mem

let empty_mem = []

(***************************)
(*     Handling record     *)
(***************************)

let rec lookup_record : id -> record -> loc
= fun id record -> 
  match record with
    | [] -> raise(Failure ("field "^ id ^" is not included in record"))
    | (x,l)::tl -> if(id=x) then l else lookup_record id tl


let extend_record : (id * loc) -> record -> record
= fun (x,l) record -> (x,l)::record

let empty_record = []

(***************************)

let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

let rec list_fold2 : ('a -> 'b -> 'c -> 'c)-> 'a list -> 'b list -> 'c -> 'c
= fun func l1 l2 acc ->
  match (l1,l2) with
  | ([],[]) -> acc
  | (hd1::tl1,hd2::tl2) -> list_fold2 func tl1 tl2 (func hd1 hd2 acc)
  | _ -> raise (Failure "two lists have different length")

let rec list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun func l acc ->
  match l with
  | [] -> acc
  | hd::tl -> list_fold func tl (func hd acc)

let value2str : value -> string
= fun v ->
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "unit"
  | Record _ -> "record" 

(* My functions *)
let rec list_last : 'a list -> 'a
= fun lst ->
  match lst with
  | [] -> raise (Failure "Empty list")
  | [hd] -> hd
  | hd::tl -> list_last tl

let rec list_first : 'a list -> 'a
= fun lst ->
  match lst with
  | [] -> raise (Failure "Empty list")
  | hd::_ -> hd

let rec list_reverse : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> raise (Failure "Empty list")
  | hd::[] -> [hd]
  | hd::tl -> (list_reverse tl) @ [hd]

let rec list_extract_first : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> raise (Failure "Empty list")
  | hd::[] -> [hd]
  | hd::tl -> tl

let rec list_extract_last : 'a list -> 'a list
= fun lst ->
  let lst_rev = (list_reverse lst) in
  (
    list_reverse (list_extract_first lst_rev)
  )
(* My functions end *)

let rec eval_aop : env -> memory -> exp -> exp -> (int -> int -> int) -> (value * memory)
= fun env mem e1 e2 op ->
  let (v1,mem1) = eval env mem e1 in
  let (v2,mem2) = eval env mem1 e2 in
  match (v1,v2) with
  | (Num n1, Num n2) -> (Num (op n1 n2), mem2)
  | _ -> raise (Failure "arithmetic operation type error")

and eval : env -> memory -> exp -> (value * memory) 
=fun env mem e -> 
  match e with
  | WRITE e -> 
    let (v1,mem1) = eval env mem e in
    let _ = print_endline(value2str v1) in
    (v1,mem1)
  | TRUE -> (Bool true, mem)
  | FALSE -> (Bool false, mem)
  | NUM n -> (Num n, mem)
  | UNIT -> (Unit, mem)
  | VAR id -> (lookup_mem (lookup_loc_env id env) mem, mem)
  | ADD (exp1, exp2) -> eval_aop env mem exp1 exp2 (fun x y -> x + y)
  | SUB (exp1, exp2) -> eval_aop env mem exp1 exp2 (fun x y -> x - y)
  | MUL (exp1, exp2) -> eval_aop env mem exp1 exp2 (fun x y -> x * y)
  | DIV (exp1, exp2) -> 
    let v2 = (eval env mem exp2) in
    (
      match v2 with
      | (Num 0, _) -> raise UndefinedSemantics
      | _ -> eval_aop env mem exp1 exp2 (fun x y -> x / y)
    )
  | EQUAL (exp1, exp2) ->
    let (v1, mem1) = (eval env mem exp1) in
    let (v2, mem2) = (eval env mem1 exp2) in
    (
      match v1, v2 with
      | Num n1, Num n2 -> 
          if n1 = n2 then (Bool true, mem2) else (Bool false, mem2)
      | Bool b1, Bool b2 ->
          if b1 = b2 then (Bool true, mem2) else (Bool false, mem2)
      | Unit, Unit ->
          (Bool true, mem2)
      | _ -> (Bool false, mem2)
    )
  | LESS (exp1, exp2) ->
    let (v1, mem1) = (eval env mem exp1) in
    let (v2, mem2) = (eval env mem1 exp2) in
    (
      match v1, v2 with
      | Num n1, Num n2 ->
          if n1 < n2 then (Bool true, mem2) else (Bool false, mem2)
      | _ -> raise UndefinedSemantics
    )
  | NOT exp ->
    let (v1, mem1) = (eval env mem exp) in
    (
      match v1 with
      | Bool true -> (Bool false, mem1) 
      | Bool false -> (Bool true , mem1)
      | _ -> raise UndefinedSemantics
    )
  | SEQ (exp1, exp2) ->
    let (v1, mem1) = (eval env mem exp1) in
    let (v2, mem2) = (eval env mem1 exp2) in
      (v2, mem2)
  | IF (exp1, exp2, exp3) ->
    let (v1, mem1) = (eval env mem exp1) in
    (
      match v1 with
      | Bool true -> (eval env mem1 exp2)
      | Bool false -> (eval env mem1 exp3)
      | _ -> raise UndefinedSemantics
    )
  | WHILE (exp1, exp2) ->
    let (tf, mem0) = (eval env mem exp1) in
    (
      match tf with
      | Bool false -> (Unit, mem0)
      | Bool true ->
        let (v1, mem1) = (eval env mem0 exp2) in
        let (v2, mem2) = (eval env mem1 (WHILE(exp1, exp2))) in
          (v2, mem2)
      | _ -> raise UndefinedSemantics
    )
  | LETV (id, exp1, exp2) ->
    let (v0, mem1) = (eval env mem exp1) in
    let l_new = new_location () in
    let (v1, mem2) = (eval (extend_env (LocBind (id, l_new)) env) (extend_mem (l_new, v0) mem1) exp2) in
    (
      (v1, mem2)
    )
  | LETF (id, id_lst, exp1, exp2) ->
    let env_new = extend_env (ProcBind(id, (id_lst, exp1, env))) env in
    (
      eval env_new mem exp2
    )
  | CALLV (id_f, exp_lst) ->
  (
    (* mem_lst *)
    let mem_lst_rec_func = fun hd acc -> (let (_,m_ret) = eval env (list_last acc) hd in acc@[m_ret]) in
    let mem_lst = list_fold mem_lst_rec_func exp_lst [mem] in
    let m_n = (list_last mem_lst) in
    let mem_lst = list_extract_last mem_lst in
    (* v_lst *)
    let v_lst_rec_func = fun hd1 hd2 acc -> (let (v_ret,_) = eval env hd1 hd2 in acc@[v_ret]) in
    let v_lst = list_fold2 v_lst_rec_func mem_lst exp_lst [] in
    (* env_f *)
    let env_f = (lookup_proc_env id_f env) in
    let (x_lst, e_p, env_p) = env_f in
    (* l_lst *)
    let l_lst_rec_func = fun hd acc -> acc@[new_location ()] in
    let l_lst = list_fold l_lst_rec_func v_lst [] in
    (* x_to_l *)
    let env_binding_lst_rec_func = fun hd1 hd2 acc -> acc@[LocBind (hd1, hd2)] in
    let x_to_l_binding_lst = list_fold2 env_binding_lst_rec_func x_lst l_lst [] in
    (* l_to_v *)
    let mem_binding_lst_rec_func = fun hd1 hd2 acc -> acc@[(hd1, hd2)] in
    let l_to_v_binding_lst = list_fold2 mem_binding_lst_rec_func l_lst v_lst [] in
    (* env_new *)
    let env_extend_rec_func = fun hd acc -> extend_env hd acc in
    let env_new = list_fold env_extend_rec_func x_to_l_binding_lst env_p in
    let env_new = extend_env (ProcBind (id_f, env_f)) env_new in
    (* mem_new *)
    let mem_new_rec_func = fun hd acc -> extend_mem hd acc in
    let mem_new = list_fold mem_new_rec_func l_to_v_binding_lst m_n in
      eval env_new mem_new e_p
  )
  | CALLR (id_f, id_y_lst) ->
    (* env_f *)
    let env_f = (lookup_proc_env id_f env) in
    let (x_lst, e, env_p) = env_f in
    (* env_y_lst *)
    let env_y_lst_rec_func = fun hd acc -> acc@[(lookup_loc_env hd env)] in
    let env_y_lst = list_fold env_y_lst_rec_func id_y_lst [] in
    (* x_to_env_y *)
    let x_to_env_y_binding_lst_rec_func = fun hd1 hd2 acc -> acc@[LocBind (hd1, hd2)] in
    let x_to_env_y_binding_lst = list_fold2 x_to_env_y_binding_lst_rec_func x_lst env_y_lst [] in
    (* env_new *)
    let env_new_rec_func = fun hd acc -> extend_env hd acc in
    let env_new = list_fold env_new_rec_func x_to_env_y_binding_lst env_p in
    let env_new = extend_env (ProcBind(id_f, env_f)) env_new in
    (
      eval env_new mem e
    )
  | RECORD x_e_lst ->   (* (id, exp) lst *)
  (
    match x_e_lst with
    | [] -> (Unit, mem)
    | _ ->
    (
      (* x_lst *)
      let x_lst_rec_func = fun hd acc -> (let (x_ret,_) = hd in acc@[x_ret]) in
      let x_lst = list_fold x_lst_rec_func x_e_lst [] in
      (* e_lst *)
      let e_lst_rec_func = fun hd acc -> (let (_,e_ret) = hd in acc@[e_ret]) in
      let e_lst = list_fold e_lst_rec_func x_e_lst [] in
      (* mem_lst *)
      let mem_lst_rec_func = fun hd acc -> (let (_,m_ret) = eval env (list_last acc) hd in acc@[m_ret]) in
      let mem_lst = list_fold mem_lst_rec_func e_lst [mem] in
      let m_n = (list_last mem_lst) in
      let mem_lst = list_extract_last mem_lst in
      (* v_lst *)
      let v_lst_rec_func = fun hd1 hd2 acc -> (let (v_ret,_) = eval env hd1 hd2 in acc@[v_ret]) in
      let v_lst = list_fold2 v_lst_rec_func mem_lst e_lst [] in
      (* l_lst *)
      let l_lst_rec_func = fun hd acc -> acc@[new_location ()] in
      let l_lst = list_fold l_lst_rec_func x_lst [] in
      (* x_to_l *)
      let env_binding_lst_rec_func = fun hd1 hd2 acc -> acc@[(hd1, hd2)] in
      let x_to_l_binding_lst = list_fold2 env_binding_lst_rec_func x_lst l_lst [] in
      (* l_to_v *)
      let mem_binding_lst_rec_func = fun hd1 hd2 acc -> acc@[(hd1, hd2)] in
      let l_to_v_binding_lst = list_fold2 mem_binding_lst_rec_func l_lst v_lst [] in
      (* mem_new *)
      let mem_new_rec_func = fun hd acc -> extend_mem hd acc in
      let mem_new = list_fold mem_new_rec_func l_to_v_binding_lst m_n in
      (
        (Record (x_to_l_binding_lst), mem_new)
      )
    )
  )
  | FIELD (exp, id_x) ->    (* access record field *)
  (
    let (r, m_p) = (eval env mem exp) in
    match r with
    | Record rec_r ->
      let r_x = (lookup_record id_x rec_r) in
      let v = (lookup_mem r_x m_p) in
      (
        (v, m_p)
      )
    | _ -> raise UndefinedSemantics
  )
  | ASSIGN (id_x, exp) ->
    let (v, mem1) = (eval env mem exp) in
    let mem_new = extend_mem ((lookup_loc_env id_x env), v) mem1 in
      (v, mem_new)
  | ASSIGNF (exp1, id, exp2) ->
    let (v0, mem1) = (eval env mem exp1) in
    let (v, mem2) = (eval env mem1 exp2) in
    (
      match v0 with
      | Record r -> (v, extend_mem ((lookup_record id r), v) mem2)
      | _ -> raise UndefinedSemantics
    )

let runb : exp -> value 
=fun exp -> let (v, _) = eval empty_env empty_mem exp in v;;

(*
runb (
  LETV ("ret", NUM 1,
    LETV ("n", NUM 5,
      SEQ (
        WHILE (LESS (NUM 0, VAR "n"),
          SEQ (
            ASSIGN ("ret", MUL (VAR "ret", VAR "n")),
            ASSIGN ("n", SUB (VAR "n", NUM 1))
          )
        ),
        VAR "ret")))
);;

result = Num 120

runb (
  LETF ("f", ["x1"; "x2"],
    SEQ (
        ASSIGN ("x1", NUM 3),
        ASSIGN ("x2", NUM 3)
    ),
    LETV("x1", NUM 1,
      LETV("x2", NUM 1,
        SEQ(
            CALLR ("f", ["x1"; "x2"]),
            ADD(VAR "x1", VAR "x2")))))
);;

result = Num 6

runb (
  LETV ("f", RECORD ([("x", NUM 10); ("y", NUM 13)]),
    LETF ("swap", ["a"; "b"],
      LETV ("temp", VAR "a",
        SEQ (
          ASSIGN ("a", VAR "b"),
          ASSIGN ("b", VAR "temp"))),
      SEQ (
        CALLV("swap", [FIELD (VAR "f", "x"); FIELD (VAR "f", "y")]),
        FIELD (VAR "f", "x")
      )
    )
  )
);;

result = Num 10

*)