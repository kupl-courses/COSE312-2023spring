open Lib.Util

type program = linstr list
and linstr = label * instr                  (* labeled instruction *)
and instr = 
  | SKIP
  | FUNC_DEF of id * id list * linstr list  (* def f(args): body *)
  | CALL of id * id * id list               (* x = call(f, args )*)
  | RETURN of id                            (* return x *) 
  | RANGE of id * id * id                   (* x = range(lo, hi) *)
  | LIST_EMPTY of id                        (* x = [] *)
  | LIST_APPEND of id * id                  (* append(x,y) *)
  | LIST_INSERT of id * id                  (* insert(x,y) *)
  | LIST_REV of id                          (* reverse(x) *)
  | TUPLE_EMPTY of id                       (* x = () *)
  | TUPLE_INSERT of id * id                 (* tupinsert(x,y) *)
  | ITER_LOAD of id * id * id               (* x = a[y] *)
  | ITER_STORE of id * id * id              (* a[x] = y *)
  | ITER_LENGTH of id * id                  (* x = len(y) *)
  | ASSIGNV of id * bop * id * id           (* x = y bop z *)
  | ASSIGNC of id * bop * id * int          (* x = y bop n *)
  | ASSIGNU of id * uop * id                (* x = uop y *)
  | COPY of id * id                         (* x = y *)
  | COPYC of id * int                       (* x = n *)
  | COPYS of id * string                    (* x = s *)
  | COPYN of id                             (* x = None *)
  | UJUMP of label                          (* goto L *)
  | CJUMP of id * label                     (* if x goto L *)
  | CJUMPF of id * label                    (* ifFalse x goto L *)
  | READ of id                              (* read x *)
  | WRITE of id                             (* write x *)
  | INT_OF_STR of id * id                   (* x = int(y) *)
  | IS_INSTANCE of id * id * string         (* x = isinstance(y, typ) *)
  | ASSERT of id                            (* assert x *)
  | HALT
and id = string
and label = int
and bop = ADD | SUB | MUL | DIV | MOD | POW | LT | LE | GT | GE | EQ | NEQ | AND | OR
and uop = UPLUS | UMINUS | NOT

(*************************************)
(*       to_string functions         *)
(*************************************)
let string_of_label l = Printf.sprintf ("%4d") l 

let string_of_bop o = 
  match o with 
  | ADD -> "+" 
  | SUB -> "-" 
  | MUL -> "*" 
  | DIV -> "/" 
  | MOD -> "%"
  | POW -> "**"
  | LT -> "<" 
  | LE -> "<=" 
  | GT -> ">" 
  | GE -> ">=" 
  | EQ -> "==" 
  | NEQ -> "!=" 
  | AND -> "&&" 
  | OR -> "||" 

let string_of_uop o = 
  match o with 
  | UMINUS -> "-" 
  | UPLUS -> "+" 
  | NOT -> "!" 

let rec tab indent = 
  if indent <= 0 then ""
  else "    " ^ tab (indent-1)
  
let rec string_of_instr : int -> instr -> string
=fun indent instr -> 
  match instr with
  | HALT -> "HALT"
  | SKIP -> "SKIP"
  | FUNC_DEF (f, args, body) -> 
    ("def " ^ f ^ string_of_list ~sep:", " id args) ^ "\n" ^
    string_of_linstrs (indent+1) body
  | RETURN x -> "return " ^ x
  | COPYN x -> x ^ " = None"
  | CALL (x, f, args) -> x ^ " := call(" ^ f ^ ", " ^ string_of_list id args ^ ")"
  | ASSIGNV (x,o,y,z) -> x ^ " = " ^ y ^ " " ^ string_of_bop o ^ " " ^ z
  | ASSIGNC (x,o,y,n) -> x ^ " = " ^ y ^ " " ^ string_of_bop o ^ " " ^ string_of_int n
  | ASSIGNU (x,o,y) -> x ^ " = " ^ string_of_uop o ^ y
  | COPY (x,y) -> x ^ " = " ^ y
  | COPYC (x,n) -> x ^ " = " ^ string_of_int n
  | COPYS (x,s) -> x ^ " = " ^ "\"" ^ String.escaped s ^ "\""
  | UJUMP label -> "goto " ^ string_of_int label
  | CJUMP (x,l) -> "if " ^ x ^ " goto " ^ string_of_int l
  | CJUMPF (x,l) -> "iffalse " ^ x ^ " goto " ^ string_of_int l
  | READ x -> "read " ^ x
  | RANGE (x, y, z) -> x ^ " = " ^ " range(" ^ y ^ ", " ^ z ^ ")"
  | LIST_EMPTY x -> x ^ " = []"
  | LIST_APPEND (x,y) -> x ^ " = " ^ x ^ "@[" ^ y ^ "]"
  | LIST_INSERT (x,y) -> x ^ " = " ^ y ^ "::" ^ x ^ ""
  | TUPLE_EMPTY x -> x ^ " = ()"
  | TUPLE_INSERT (x,y) -> x ^ " = (" ^ y ^ ") + " ^ x
  | ITER_LOAD (x,l,y) -> x ^ " = " ^ l ^ "[" ^ y ^ "]"
  | ITER_STORE (l,x,y) -> l ^ "[" ^ x ^ "] = " ^ y
  | LIST_REV x -> "LIST_REV " ^ x
  | ITER_LENGTH (x,y) -> x ^ " = len(" ^ y ^ ")"
  | INT_OF_STR (x,y) -> x ^ " = int(" ^ y ^ ")"
  | ASSERT x -> "assert " ^ x
  | IS_INSTANCE (x, y, typ) -> x ^ " = isinstance(" ^ y ^ ", " ^ typ ^ ")"
  | WRITE x -> "write " ^ x

and string_of_linstrs : int -> linstr list -> string
=fun indent linstrs -> 
  list_fold (fun (label, instr) str ->
    str ^ tab indent ^ string_of_label label ^ " : " ^ string_of_instr indent instr ^ "\n"
  ) linstrs ""

and string_of_program program = string_of_linstrs 0 program 

(*************************************)
(*           Machine State           *)
(*************************************)
exception AssertionFailure of string 
exception TypeError of string 

type loc = int 
and value = NONE | NUM of int | STR of string | REF of loc | TUPLE of value list | LIST of value list | CLOS of id * id list * linstr list
and typ = TY_NONE | TY_NUM | TY_STR | TY_TUPLE | TY_REF | TY_LIST | TY_FUNC

let string_of_loc loc = string_of_int loc 

let rec string_of_value v = 
  match v with
  | NONE -> "None"
  | NUM n -> string_of_int n
  | STR s -> s 
  | REF loc -> "ref " ^ string_of_int loc
  | TUPLE vs -> string_of_list ~first:"(" ~last:")" ~sep:", " string_of_value vs
  | LIST vs -> string_of_list ~first:"[" ~last:"]" ~sep:", " string_of_value vs
  | CLOS (f, args, _) -> "<function " ^ f ^ " " ^ string_of_list id args ^ ">"

let new_loc = ref 1

module Memory = struct
  type t = (loc, value) BatMap.t 
  let empty = BatMap.empty 
  let bind = BatMap.add
  let lookup a m = try BatMap.find a m with _ -> raise (Failure "Memory error")
  let alloc_new_loc : t -> t * loc 
  =fun mem -> 
    new_loc := !new_loc + 1; 
    let loc = !new_loc in 
      (bind loc (NUM 0) mem, loc)
end

module Env = struct
  type t = (id, loc) BatMap.t
  let empty = BatMap.empty
  let bind = BatMap.add 
  let bound : id -> t -> bool 
  =fun x m -> BatMap.exists (fun k _ -> k = x) m 
  let lookup : id -> t -> loc 
  =fun x m -> try BatMap.find x m with _ -> raise (Failure ("Env: " ^ x ^ " not found "))
end

module StackFrame = struct 
  type t = func_name * return_label * return_addr * Env.t 
  and func_name = string
  and return_label = label option
  and return_addr = loc option 

  let create (f, l, a, e) = (f, l, a, e)

  let bind_env x loc (f, l, a, env) = (f, l, a, Env.bind x loc env)
  let lookup_env x (_, _, _, env) = Env.lookup x env
  let bound_env x (_, _, _, env) = Env.bound x env 

  let get_func_name (f,_,_,_) = f 
  let get_return_label (_,l_opt,_,_) = 
    match l_opt with 
    | None -> raise (Failure "StackFrame.get_return_label")
    | Some l -> l
  let get_return_addr (_,_,a_opt,_) = 
    match a_opt with 
    | None -> raise (Failure "StackFrame.get_return_addr")
    | Some a -> a
  let get_env (_,_,_,env) = env
end

module CallStack = struct
  type t = StackFrame.t list 
  let empty = []
  let push : StackFrame.t -> t -> t
  =fun frame stack -> frame::stack 
  let pop stack = 
    match stack with 
    | [] -> raise (Failure "Call stack is empty")
    | hd::tl -> (hd, tl)
  let bind_env x loc stack = 
    match stack with 
    | [] -> raise (Failure "Call stack is empty (bind_env)")
    | hd::tl -> StackFrame.bind_env x loc hd::tl 
  let rec lookup_env x stack = 
    match stack with 
    | [] -> raise (Failure ("Call stack is empty (lookup_env): " ^ x))
    | hd::tl -> 
      try 
        StackFrame.lookup_env x hd
      with _ -> lookup_env x tl 
  let bound_env x stack = 
    match stack with 
    | [] -> raise (Failure "Call stack is empty (bound_env)")
    | hd::_ -> StackFrame.bound_env x hd
end

type pgm_info = {
  pgm : program; 
  succ_map : (label, label option) BatMap.t; 
  instructions : linstr list 
}

type state = {
  pc : label; 
  callstack : CallStack.t; 
  memory : Memory.t; 
  pgm_info : pgm_info
}

(*************************************)
(*              Executor             *)
(*************************************)
let rec build_succ_map : linstr list -> (label, label option) BatMap.t 
=fun instrs -> 
  let zipped = zip instrs (List.tl instrs) in 
  let map1 = 
    list_fold (fun (a_opt, b_opt) map ->
      match a_opt, b_opt with 
      | Some (a, _), Some (b, _) -> BatMap.add a (Some b) map 
      | Some (a, _), None -> BatMap.add a None map 
      | _ -> raise (Failure "build_succ_map")
    ) zipped BatMap.empty in 
  let map2 = 
    list_fold (fun (_, instr) map -> 
      match instr with 
      | FUNC_DEF (_, _, body) -> 
        BatMap.foldi (fun k v m -> 
          BatMap.add k v m 
        ) (build_succ_map body) map 
      | _ -> map
    ) instrs map1 in
    map2 
    
let rec collect_instrs : linstr list -> linstr list 
=fun instrs -> 
  match instrs with 
  | [] -> []
  | ((_, FUNC_DEF(_,_,body)) as hd)::tl -> [hd] @ collect_instrs body @ collect_instrs tl
  | hd::tl -> [hd] @ collect_instrs tl 

let get_instr state = List.assoc state.pc state.pgm_info.instructions 

let get_next_pc state = 
  match BatMap.find state.pc state.pgm_info.succ_map with 
  | None -> raise (Failure "No next program counter")
  | Some next_pc -> next_pc

let get_value id state = 
  let f_loc = CallStack.lookup_env id state.callstack in 
    Memory.lookup f_loc state.memory

let bind_arguments formals actual_values state = 
  list_fold (fun (formal_opt, actual_value_opt) state ->
    match formal_opt, actual_value_opt with 
    | Some formal, Some actual_value -> 
      let mem, loc = Memory.alloc_new_loc state.memory in 
        {state with 
          callstack = CallStack.bind_env formal loc state.callstack; 
          memory = Memory.bind loc actual_value mem 
        }
    | _ -> raise (TypeError "CALL (argument mismatch)")
    ) (zip formals actual_values) state

let allocate_new_stack_frame f_name ret_label ret_var_loc state = 
  let frame = StackFrame.create (f_name, Some ret_label, Some ret_var_loc, Env.empty) in 
    {state with 
      callstack = CallStack.push frame state.callstack; 
    }

let update_pc : label -> state -> state 
=fun label state -> {state with pc = label}

let lookup_with_alloc : id -> state -> loc * state 
=fun x state -> 
  if CallStack.bound_env x state.callstack = false then 
    let mem, loc_x = Memory.alloc_new_loc state.memory in 
    let callstack = CallStack.bind_env x loc_x state.callstack in 
      (loc_x, {state with callstack=callstack; memory=mem})
  else
    (CallStack.lookup_env x state.callstack, state)

let rec pow n1 n2 = 
  if n2 = 0 then 1 
  else n1 * pow n1 (n2-1)

let eval_binary_primitive : value -> bop -> value -> value
=fun v1 op v2 ->
  match v1, op, v2 with
  | NUM n1, ADD, NUM n2 -> NUM (n1+n2)
  | NUM n1, SUB, NUM n2 -> NUM (n1-n2)
  | NUM n1, MUL, NUM n2 -> NUM (n1*n2)
  | NUM n1, DIV, NUM n2 -> NUM (n1/n2)
  | NUM n1, MOD, NUM n2 -> NUM (let r = n1 mod n2 in if r < 0 then r + n2 else r)
  | NUM n1, POW, NUM n2 -> NUM (pow n1 n2)
  | NUM n1, LT, NUM n2 -> if n1 < n2 then NUM 1 else NUM 0
  | NUM n1, LE, NUM n2 -> if n1 <= n2 then NUM 1 else NUM 0
  | NUM n1, GT, NUM n2 -> if n1 > n2 then NUM 1 else NUM 0
  | NUM n1, GE, NUM n2 -> if n1 >= n2 then NUM 1 else NUM 0
  | NUM n1, EQ, NUM n2 -> if n1 = n2 then NUM 1 else NUM 0
  | NUM n1, NEQ, NUM n2 -> if n1 != n2 then NUM 1 else NUM 0
  | NUM n1, AND, NUM n2 -> if n1 != 0 && n2 != 0 then NUM 1 else NUM 0
  | NUM n1, OR, NUM n2 -> if n1 != 0 || n2 != 0 then NUM 1 else NUM 0
  | NUM _, EQ, NONE -> NUM 0
  | NONE, EQ, NUM _ -> NUM 0
  | NONE, EQ, NONE -> NUM 1
  | STR s1, ADD, STR s2 -> STR (s1 ^ s2)
  | NUM n, MUL, STR s  
  | STR s, MUL, NUM n -> 
    let rec repeat s n = 
      if n <= 0 then "" else s ^ repeat s (n-1) in 
        STR (repeat s n)
  | _ -> raise (TypeError ("eval_binary_primitive" ^ string_of_value v1 ^ string_of_bop op ^ string_of_value v2))

let eval_binary_ref : value -> bop -> value -> state -> value * state
=fun v1 op v2 state ->
  match v1, op, v2 with
  | REF l1, ADD, REF l2 -> 
    begin
      match Memory.lookup l1 state.memory, Memory.lookup l2 state.memory with 
      | LIST lst1, LIST lst2 -> 
        let mem, loc = Memory.alloc_new_loc state.memory in 
        let mem = Memory.bind loc (LIST (lst1 @ lst2)) mem in 
          (REF loc, {state with memory = mem})
      | _ -> raise (TypeError "eval_binary_ref: ADD")
    end
  | REF l1, EQ, REF l2 -> 
    begin
      match Memory.lookup l1 state.memory, Memory.lookup l2 state.memory with 
      | LIST lst1, LIST lst2 -> ((if lst1 = lst2 then NUM 1 else NUM 0), state)
      | _ -> raise (TypeError "eval_binary_ref: EQ")
    end
  | _ -> raise (TypeError ("eval_binary_ref" ^ string_of_value v1 ^ string_of_bop op ^ string_of_value v2))

let eval_unary : uop -> value -> value
=fun op v ->
  match op, v with
  | UPLUS, NUM n -> NUM (n)
  | UMINUS, NUM n -> NUM (-n)
  | NOT, NUM n -> if n = 0 then NUM 1 else NUM 0
  | _ -> raise (TypeError "eval_unary")

let eval_var : id -> state -> value
=fun x state -> Memory.lookup (CallStack.lookup_env x state.callstack) state.memory

let eval_ref : id -> state -> loc * value 
=fun x state -> 
  match eval_var x state with 
  | REF loc_x -> (loc_x, Memory.lookup loc_x state.memory)
  | _ -> raise (TypeError ("eval_ref: " ^ x ^ " is not a reference"))

let typeof : id -> state -> typ  
=fun x state -> 
  match eval_var x state with 
  | NONE -> TY_NONE
  | NUM _ -> TY_NUM
  | STR _ -> TY_STR 
  | REF _ -> TY_REF 
  | TUPLE _ -> TY_TUPLE
  | LIST _ -> TY_LIST
  | CLOS _ -> TY_FUNC 

let is_primitive_value : id -> state -> bool 
=fun x state -> List.mem (typeof x state) [TY_NONE; TY_NUM; TY_STR]

let move : state -> state option 
=fun state -> 
  match get_instr state with 
  | FUNC_DEF (f, args, body) -> 
    let mem, loc = Memory.alloc_new_loc state.memory in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc (CLOS (f, args, body)) mem; 
        callstack = CallStack.bind_env f loc state.callstack 
      }
  | CALL (ret_var, f, actuals) -> 
    let actual_values = List.map (fun x -> eval_var x state) actuals in 
    let ret_label = 
      match BatMap.find state.pc state.pgm_info.succ_map with 
      | Some label -> label 
      | None -> raise (Failure "No return label") in 
    let mem, ret_var_loc = Memory.alloc_new_loc state.memory in 
    let state = 
      {state with 
        memory = mem; 
        callstack = CallStack.bind_env ret_var ret_var_loc state.callstack} in 
      begin 
        match get_value f state with 
        | CLOS (f_name, formals, (entry,_)::_) -> 
          Some (state 
          |> allocate_new_stack_frame f_name ret_label ret_var_loc 
          |> bind_arguments formals actual_values 
          |> update_pc entry)
        | _ -> raise (TypeError "CALL: not a function value")
      end
  | RETURN x -> 
    let frame, callstack = CallStack.pop state.callstack in 
    let loc_x = StackFrame.lookup_env x frame in 
    let val_x = Memory.lookup loc_x state.memory in 
    let ret_addr = StackFrame.get_return_addr frame in 
    let ret_label = StackFrame.get_return_label frame in 
      Some {state with 
        pc = ret_label; 
        callstack = callstack; 
        memory = Memory.bind ret_addr val_x state.memory;
      }
  | RANGE (x, y, z) -> 
    let loc_x, state = lookup_with_alloc x state in 
    begin 
      match eval_var y state, eval_var z state with 
      | NUM lo, NUM hi -> 
        Some {state with 
          pc = get_next_pc state; 
          memory = 
            let mem, loc_l = Memory.alloc_new_loc state.memory in 
            let lst = List.map (fun n -> NUM n) (range lo (hi-1)) in 
              mem 
              |> Memory.bind loc_x (REF loc_l) 
              |> Memory.bind loc_l (LIST lst)
        }
      | _ -> raise (TypeError "RANGE")
    end
  | LIST_EMPTY x -> 
    let loc_x, state = lookup_with_alloc x state in 
      Some {state 
        with 
          pc = get_next_pc state; 
          memory = 
          let mem, loc_l = Memory.alloc_new_loc state.memory in 
            mem 
            |> Memory.bind loc_x (REF loc_l)
            |> Memory.bind loc_l (LIST [])
      }      
  | LIST_REV x -> 
    begin 
      match eval_ref x state with 
      | loc, LIST lst -> 
        Some {state with 
          pc = get_next_pc state; 
          memory = Memory.bind loc (LIST (List.rev lst)) state.memory
        }
      | _ -> raise (TypeError "LIST_REV")
    end 
  | LIST_APPEND (x, y) -> 
    begin 
      match eval_ref x state with 
      | loc_x, LIST lst_x -> 
        Some {state with 
          pc = get_next_pc state; 
          memory = Memory.bind loc_x (LIST (lst_x @ [eval_var y state])) state.memory
        }
      | _ -> raise (TypeError "LIST_APPEND")
    end 
  | LIST_INSERT (x, y) -> 
      begin 
        match eval_ref x state with 
        | loc_x, LIST lst_x -> 
          Some {state with 
            pc = get_next_pc state; 
            memory = Memory.bind loc_x (LIST ((eval_var y state)::lst_x)) state.memory
          }
        | _ -> raise (TypeError "LIST_INSERT")
      end 
  | TUPLE_EMPTY x -> 
    let loc_x, state = lookup_with_alloc x state in 
      Some {state 
        with 
          pc = get_next_pc state; 
          memory = Memory.bind loc_x (TUPLE []) state.memory 
          (* let mem, loc_l = Memory.alloc_new_loc state.memory in 
            mem 
            |> Memory.bind loc_x (REF loc_l)
            |> Memory.bind loc_l (TUPLE []) *)
      }      
  | TUPLE_INSERT (x, y) -> 
(*    let loc_x, state = lookup_with_alloc x state in  *)
    let loc_x = CallStack.lookup_env x state.callstack in 
    begin 
      match eval_var x state with 
      | TUPLE lst_x -> 
        Some {state with 
          pc = get_next_pc state; 
          memory = Memory.bind loc_x (TUPLE ((eval_var y state)::lst_x)) state.memory
        }
      | _ -> raise (TypeError "TUPLE_INSERT")
      (* match eval_ref x state with 
      | loc_x, TUPLE lst_x -> 
        Some {state with 
          pc = get_next_pc state; 
          memory = Memory.bind loc_x (TUPLE ((eval_var y state)::lst_x)) state.memory
        }
      | _ -> raise (TypeError "TUPLE_INSERT") *)
    end 
  | ITER_LOAD (x, l, y) when typeof l state = TY_REF -> 
    let loc_x, state = lookup_with_alloc x state in 
      begin 
        match eval_ref l state, eval_var y state with 
        | (_, LIST lst), NUM n -> 
          Some {state with 
            pc = get_next_pc state;
            memory = Memory.bind loc_x (List.nth lst n) state.memory
          }
        | _ -> raise (TypeError "ITER_LOAD")
      end
  | ITER_LOAD (x, l, y) when typeof l state = TY_STR -> 
    let loc_x, state = lookup_with_alloc x state in 
      begin 
        match eval_var l state, eval_var y state with 
        | STR str, NUM n -> 
          Some {state with 
            pc = get_next_pc state;
            memory = Memory.bind loc_x (STR (String.sub str n 1)) state.memory
          }
        | _ -> raise (TypeError "ITER_LOAD")
      end
  | ITER_LOAD (x, l, y) when typeof l state = TY_TUPLE -> 
    let loc_x, state = lookup_with_alloc x state in 
      begin 
        match eval_var l state, eval_var y state with 
        | TUPLE tup, NUM n -> 
          Some {state with 
            pc = get_next_pc state;
            memory = Memory.bind loc_x (List.nth tup n) state.memory
          }
        | _ -> raise (TypeError "ITER_LOAD")
      end
  | ITER_STORE (l, x, y) -> 
    begin 
      match eval_ref l state, eval_var x state with 
      | (loc, LIST lst), NUM n -> 
        let lst' = list_replace lst n (eval_var y state) in 
          Some {state with 
            pc = get_next_pc state; 
            memory = Memory.bind loc (LIST lst') state.memory 
          }
      | _ -> raise (TypeError "ITER_STORE")
    end
  | ITER_LENGTH (x, l) when typeof l state = TY_STR-> 
    let loc_x, state = lookup_with_alloc x state in 
    begin 
      match eval_var l state with 
      | STR str -> 
        Some {state with 
                pc = get_next_pc state; 
                memory = Memory.bind loc_x (NUM (String.length str)) state.memory
        }
      | _ -> raise (TypeError "LENGTH: not a string")
    end 
  | ITER_LENGTH (x, l) when typeof l state = TY_REF -> 
      let loc_x, state = lookup_with_alloc x state in 
      begin 
        match eval_ref l state with 
        | _, LIST lst -> 
          Some {state with 
            pc = get_next_pc state;
            memory = Memory.bind loc_x (NUM (List.length lst)) state.memory 
          }
        | _ -> raise (TypeError "LENGTH: not a list")
      end
  | ASSIGNV (x, bop, y, z) when is_primitive_value y state && is_primitive_value z state -> 
    let loc_x, state = lookup_with_alloc x state in 
    let val_y = eval_var y state in 
    let val_z = eval_var z state in 
    let val_x = eval_binary_primitive val_y bop val_z in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x val_x state.memory; 
      }
  | ASSIGNV (x, bop, y, z) when typeof y state = TY_REF && typeof z state = TY_REF -> 
    let loc_x, state = lookup_with_alloc x state in 
    let val_y = eval_var y state in 
    let val_z = eval_var z state in 
    let val_x, state = eval_binary_ref val_y bop val_z state in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x val_x state.memory; 
      }
  | ASSIGNC (x, bop, y, c) when is_primitive_value y state -> 
    let loc_x, state = lookup_with_alloc x state in 
    let val_y = eval_var y state in 
    let val_x = eval_binary_primitive val_y bop (NUM c) in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x val_x state.memory; 
      }
  | ASSIGNU (x, uop, y) -> 
    let loc_x, state = lookup_with_alloc x state in 
    let val_y = eval_var y state in 
    let val_x = eval_unary uop val_y in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x val_x state.memory; 
      }
  | COPY (x, y) -> 
    let loc_x, state = lookup_with_alloc x state in 
    let val_x = eval_var y state in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x val_x state.memory; 
      }
  | COPYC (x, c) -> 
    let loc_x, state = lookup_with_alloc x state in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x (NUM c) state.memory;  
      }
  | COPYS (x, s) -> 
    let loc_x, state = lookup_with_alloc x state in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x (STR s) state.memory;  
      }
  | COPYN x -> 
    let loc_x, state = lookup_with_alloc x state in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x NONE state.memory;  
      }
  | UJUMP l -> Some {state with pc = l}
  | CJUMP (x, l) -> 
    begin 
      match eval_var x state with 
      | NUM n -> Some {state with pc = if n != 0 then l else get_next_pc state}
      | _ -> raise (TypeError "CJUMP")
    end
  | CJUMPF (x, l) -> 
    begin 
      match eval_var x state with 
      | NUM n -> Some {state with pc = if n == 0 then l else get_next_pc state}
      | _ -> raise (TypeError "CJUMPF")
    end
  | SKIP -> Some {state with pc = get_next_pc state}
  | READ x -> 
    let loc_x, state = lookup_with_alloc x state in 
    let val_x = STR (read_line()) in 
      Some {state with 
        pc = get_next_pc state; 
        memory = Memory.bind loc_x val_x state.memory 
      } 
  | WRITE x -> 
    (* let string_of_value _ v = string_of_value v in  *)
    let rec string_of_value state v = 
      match v with
      | NONE -> "None"
      | NUM n -> string_of_int n
      | STR s -> s 
      | REF loc -> string_of_value state (Memory.lookup loc state.memory)
      | TUPLE vs -> string_of_list ~first:"(" ~last:")" ~sep:", " (string_of_value state) vs
      | LIST vs -> string_of_list ~first:"[" ~last:"]" ~sep:", " (string_of_value state) vs
      | CLOS (f, args, _) -> "<function " ^ f ^ " " ^ string_of_list id args ^ ">" in 
    let v_x = eval_var x state in 
    let _ = print_string (string_of_value state v_x) in 
      Some {state with pc = get_next_pc state}
  | INT_OF_STR (x, y) -> 
    let loc_x, state = lookup_with_alloc x state in 
    begin 
      match eval_var y state with 
      | STR s -> 
        Some {state with 
          pc = get_next_pc state; 
          memory = Memory.bind loc_x (NUM (int_of_string s)) state.memory}
      | _ -> raise (TypeError ("INT_OF_STR"))
    end
  | ASSERT x -> 
    begin 
      match eval_var x state with 
      | NUM n when n = 0 -> 
        raise (AssertionFailure (string_of_label state.pc ^ " : " ^ string_of_instr 0 (get_instr state)))
      | NUM n when n <> 0 -> Some {state with pc = get_next_pc state}
      | _ -> raise (TypeError "ASSERT")
    end 

  | HALT -> None 
  | IS_INSTANCE (x, y, typ) -> 
    let loc_x, state = lookup_with_alloc x state in 
    begin 
      match eval_var y state, typ with 
      | NUM _, "int" -> Some {state with pc = get_next_pc state; memory = Memory.bind loc_x (NUM 1) state.memory}
      | STR _, "str" -> Some {state with pc = get_next_pc state; memory = Memory.bind loc_x (NUM 1) state.memory}
      | REF _, "list" -> Some {state with pc = get_next_pc state; memory = Memory.bind loc_x (NUM 1) state.memory}
      | _ -> Some {state with pc = get_next_pc state; memory = Memory.bind loc_x (NUM 0) state.memory}
    end
  | _ -> raise (Failure ("move: must not happen " ^ string_of_instr 0 (get_instr state)))

let rec loop state count = 
  match move state with 
  | None -> print_endline ("The number of instructions executed : " ^ string_of_int count)
  | Some state' -> loop state' (count+1)

let check_uniq_of_labels _ = true (* TODO *)

let execute : program -> unit
=fun pgm -> 
  assert (check_uniq_of_labels pgm); 
  let pgm_info = {
    pgm = pgm; 
    succ_map = build_succ_map pgm;
    instructions = collect_instrs pgm; } in 
  let init_label = 
    match pgm with 
    | (label, _)::_ -> label 
    | _ -> raise (Failure "No instruction") in 
  let frame0 = StackFrame.create ("__main__", None, None, Env.empty) in
  let state0 = {
    pc = init_label; 
    callstack = CallStack.push frame0 CallStack.empty; 
    memory = Memory.empty;
    pgm_info = pgm_info } in 
    loop state0 1