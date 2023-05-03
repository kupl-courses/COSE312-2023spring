open Spy
open Lib.Util
open Spvm

let temp_var_index = ref 0
let label_index = ref 1
let new_temp_var() = temp_var_index := !temp_var_index + 1; ".t" ^ (string_of_int !temp_var_index)
let new_label() = label_index := !label_index + 1; !label_index

exception Not_Implemented of string 
exception Error of string 

type ctx = {
  loop_entry : Spvm.label option; 
  loop_exit : Spvm.label option
}

let ctx0 = {
  loop_entry = None; 
  loop_exit = None
}

let rec translate : Spy.program -> Spvm.program
=fun stmts -> translate_stmts ctx0 stmts @ [new_label(), HALT]

and translate_stmts : ctx -> Spy.stmt list -> Spvm.linstr list  
=fun ctx stmts -> list_fold (fun s a -> a @ (translate_stmt ctx s)) stmts []

and translate_expr : Spy.expr -> Spvm.id * Spvm.linstr list 
=fun expr -> 
  match expr with
  | Name id -> 
    let t = new_temp_var() in 
    let code = [new_label(), COPY(t, id)] in
      (t, code)
  | Constant c -> 
    let t = new_temp_var() in
    begin 
      match c with 
      | CInt n -> (t, [new_label(), COPYC(t, n)])
      | CBool b -> (t, [new_label(), COPYC(t, if b then 1 else 0)])
      | CString s -> (t, [new_label(), COPYS(t, s)])
      | CNone -> (t, [new_label(), COPYN t])
    end
  | UnaryOp (uop, operand) -> 
    let t = new_temp_var() in 
    let (x, code) = translate_expr operand in
    let op = translate_uop uop in 
      (t, code @ [new_label(), ASSIGNU(t, op, x)])
  | BinOp (left, op, right) -> 
    let t = new_temp_var() in 
    let (x1, code1) = translate_expr left in 
    let (x2, code2) = translate_expr right in 
    let op = translate_bop op in 
      (t, code1 @ code2 @ [(new_label(), ASSIGNV(t, op, x1, x2))])
  | Compare (left, op, right) -> 
    let t = new_temp_var() in 
    let (x1, code1) = translate_expr left in 
    let (x2, code2) = translate_expr right in 
    let op = translate_cmpop op in 
      (t, code1 @ code2 @ [(new_label(), ASSIGNV(t, op, x1, x2))])   
  | BoolOp (op, exprs) -> (* TODO: short-circuit *)
    let t = new_temp_var() in 
    let bop : Spvm.bop = translate_boolop op in 
      begin
        match exprs with 
        | [] -> 
          begin 
            match op with 
            | And -> (t, [new_label(), COPYC (t, 1)])
            | Or -> (t, [new_label(), COPYC (t, 0)])
          end
        | hd::tl -> 
          let (x, code1) = translate_expr hd in 
          let (y, code2) = translate_expr (BoolOp (op, tl)) in
            (t, code1 @ code2 @ [new_label(), ASSIGNV(t, bop, x, y)])
      end
  | List exprs -> 
    let t = new_temp_var() in 
      begin 
        match exprs with 
        | [] -> (t, [new_label(), LIST_EMPTY t])
        | hd::tl -> 
          let (x, code1) = translate_expr hd in 
          let (y, code2) = translate_expr (List tl) in 
          let code3 = [new_label(), LIST_INSERT (y, x)] in
            (t, code1 @ code2 @ code3 @ [new_label(), COPY (t, y)])
      end
  | Tuple exprs -> 
    let t = new_temp_var() in 
      begin 
        match exprs with 
        | [] -> (t, [new_label(), TUPLE_EMPTY t])
        | hd::tl -> 
          let (x, code1) = translate_expr hd in 
          let (y, code2) = translate_expr (Tuple tl) in 
          let code3 = [new_label(), TUPLE_INSERT (y, x)] in
            (t, code1 @ code2 @ code3 @ [new_label(), COPY (t, y)])
      end
  | Subscript (e1, e2) -> 
    let t = new_temp_var() in 
    let (x, code1) = translate_expr e1 in 
    let (y, code2) = translate_expr e2 in 
      (t, code1 @ code2 @ [new_label(), ITER_LOAD(t, x, y)])  
  | Call (f, args) when is_the_isinstance_function f && List.length args = 2 -> 
    let t = new_temp_var() in 
    let (arg1, code1) = translate_expr (List.nth args 0) in 
      begin   
        match List.nth args 1 with 
        | Name "int" -> (t, code1 @ [new_label(), IS_INSTANCE(t, arg1, "int")])
        | Name "list" -> (t, code1 @ [new_label(), IS_INSTANCE(t, arg1, "list")])
        | _ -> raise (Error "isinstance")
      end
  | Call (f, args) when is_the_range_function f && List.length args = 1 -> 
    let t = new_temp_var() in 
    let arg = List.nth args 0 in 
    let (lo, code1) = translate_expr (Constant (CInt 0)) in 
    let (hi, code2) = translate_expr arg in 
      (t, code1 @ code2 @ [new_label(), RANGE(t, lo, hi)])
  | Call (f, args) when is_the_range_function f && List.length args = 2 -> 
    let t = new_temp_var() in 
    let arg1 = List.nth args 0 in 
    let arg2 = List.nth args 1 in 
    let (lo, code1) = translate_expr arg1 in 
    let (hi, code2) = translate_expr arg2 in 
      (t, code1 @ code2 @ [new_label(), RANGE(t, lo, hi)])
  | Call (f, args) when is_the_len_function f && List.length args = 1 ->
    let t = new_temp_var() in 
    let arg = List.nth args 0 in 
    let (x, code) = translate_expr arg in 
      (t, code @ [(new_label(), ITER_LENGTH(t, x))])
  | Call (f, []) when is_the_input_function f -> 
    let t = new_temp_var() in 
      (t, [new_label(), READ t])
  | Call (f, args) when is_the_int_function f && List.length args = 1 -> 
    let t = new_temp_var() in 
    let arg = List.nth args 0 in 
    let (x, code) = translate_expr arg in 
     (t, code @ [new_label(), INT_OF_STR (t, x)])
  | Call (f, args) when is_the_print_function f -> 
    let t = new_temp_var() in 
    let print_contents = 
      list_fold (fun arg codes -> 
        let (x, code1) = translate_expr arg in 
        let (y, code2) = translate_expr (Constant (CString " ")) in 
          codes @ code1 @ code2 @ [new_label(), WRITE x; new_label(), WRITE y]
      ) args [] in 
    let print_newline = 
      let (x, code) = translate_expr (Constant (CString "\n")) in 
        code @ [new_label(), WRITE x] in 
      (t, print_contents @ print_newline @ [new_label(), COPYN t])
  | Call (f, args) when is_the_append_function f && List.length args = 1 -> 
    let t = new_temp_var() in 
    let arg = List.nth args 0 in 
    let v = get_attribute_value f in 
    let (x, code1) = translate_expr v in 
    let (y, code2) = translate_expr arg in 
      (t, code1 @ code2 @ [new_label(), COPYN t] @ [(new_label(), LIST_APPEND (x, y))])
  | Call (f, args) -> (* user-defined functions *)
    let t = new_temp_var() in 
    let (x, code1) = translate_expr f in 
    let (ys, codes) = 
      list_fold (fun arg (vars, codes) -> 
        let (x, code) = translate_expr arg in 
          (vars@[x], codes@code)
      ) args ([],[]) in 
      (t, code1 @ codes @ [(new_label(), CALL (t, x, ys))])
  | ListComp (elt, comps) -> 
    let t = new_temp_var() in 
    let init = Assign ([Name t], List []) in 
    let body0 = Expr (Call (Attribute(Name t, "append"), [elt])) in 
    let body = 
      List.fold_right (fun (target, iter, ifs) body -> 
        For (target, iter, 
          [
            If (BoolOp (And, ifs), [body], [])  
          ]
        )
      ) comps body0 in 
    (t, translate_stmts ctx0 [init; body])  
  | Lambda (args, body) -> 
    let t = new_temp_var() in 
    let func_var = "_lambda_" ^ new_temp_var() in 
    let func_def = FunctionDef (func_var, args, [Return (Some body)]) in 
    let code = translate_stmts ctx0 [func_def; Assign ([Name t], Name func_var)] in 
      (t, code)
  (*| IfExp _ .*) 
  | _ -> raise (Not_Implemented ("translate_expr"))

and translate_bop op = 
  match op with
  | Add -> Spvm.ADD
  | Sub -> Spvm.SUB
  | Mult -> Spvm.MUL
  | Div -> Spvm.DIV
  | Mod -> Spvm.MOD 
  | Pow -> Spvm.POW

and translate_uop op = 
  match op with 
  | Not -> Spvm.NOT
  | UAdd -> Spvm.UPLUS
  | USub -> Spvm.UMINUS

and translate_cmpop op = 
  match op with
  | Eq -> Spvm.EQ
  | NotEq -> Spvm.NEQ
  | Lt -> Spvm.LT
  | LtE -> Spvm.LE
  | Gt -> Spvm.GT
  | GtE -> Spvm.GE

and translate_boolop op = 
  match op with 
  | And -> Spvm.AND 
  | Or -> Spvm.OR

and translate_stmt : ctx -> Spy.stmt -> Spvm.linstr list
=fun ctx stmt -> 
  match stmt with
  | FunctionDef (f, args, body) -> translate_fundef_stmt f args body
  | Return expr_opt -> translate_return_stmt expr_opt 
  | Assign (targets, value) -> translate_assign_stmt targets value 
  | AugAssign (target, op, value) -> translate_augassign_stmt target op value 
  | Expr expr -> translate_expr_stmt expr 
  | If (test, body, orelse) -> translate_if_stmt ctx test body orelse 
  | While (test, body) -> translate_while_stmt ctx test body 
  | For (target, iter, body) -> translate_for_stmt ctx target iter body 
  | Break -> 
    begin
      match ctx.loop_exit with 
        | Some label -> [new_label(), UJUMP label]
        | None -> raise (Failure "break: cannot happen")
    end
  | Continue -> 
    begin
      match ctx.loop_entry with 
        | Some label -> [new_label(), UJUMP label]
        | None -> raise (Failure "continue: cannot happen")
    end
  | Pass -> [new_label(), SKIP]
  | Assert expr -> 
    let (x, code) = translate_expr expr in 
      code @ [new_label(), ASSERT x]

and translate_fundef_stmt f args body = 
  [new_label(), FUNC_DEF (f, args, translate_stmts ctx0 body @ translate_stmt ctx0 (Return None))]

and translate_return_stmt expr_opt = 
  match expr_opt with 
  | Some expr -> 
    let (x, code) = translate_expr expr in 
      code @ [new_label(), RETURN x]
  | None -> 
    let (x, code) = translate_expr (Constant CNone) in 
      code @ [new_label(), RETURN x]

and translate_assign_single target rhs = 
  let (x_rhs, code_rhs) = translate_expr rhs in 
  match target with 
  | Name x -> code_rhs @ [new_label(), COPY(x, x_rhs)]
  | Subscript (e1, e2) -> 
    let (i, code1) = translate_expr e1 in 
    let (x, code2) = translate_expr e2 in 
      code_rhs @ code1 @ code2 @ [new_label(), ITER_STORE(i, x, x_rhs)]
  | Tuple lhss 
  | List lhss ->
    let _, code = 
      list_fold (fun lhs (n, code_a) -> 
        (n+1, code_a @ translate_assign_single lhs (Subscript (Name x_rhs, Constant (CInt n))))
        ) lhss (0, []) in 
      code_rhs @ code 
  | _ -> raise (Error "assignment error")

and translate_assign_stmt targets rhs = 
  list_fold (fun target code -> code @ translate_assign_single target rhs) targets []

and translate_augassign_stmt target op value = 
  translate_assign_stmt [target] (BinOp (target, op, value))
  
and translate_expr_stmt value = 
  let (_, code) = translate_expr value in 
    code
    
and translate_if_stmt ctx test body orelse = 
  let (t,code1) = translate_expr test in
  let code_true = translate_stmts ctx body in
  let code_false = translate_stmts ctx orelse in
  let label_true = new_label() in
  let label_false = new_label() in
  let label_exit = new_label() in
    code1 @ [new_label(), CJUMP (t, label_true)] @
    [new_label(), UJUMP label_false] @
    [label_true,  SKIP] @ code_true  @ [new_label(), UJUMP label_exit] @
    [label_false, SKIP] @ code_false @ [new_label(), UJUMP label_exit] @
    [label_exit, SKIP]

and translate_while_stmt _ test body = 
  let entry = new_label() in
  let exit = new_label() in
  let (t,code1) = translate_expr test in
  let code2 = translate_stmts {loop_entry=(Some entry); loop_exit=(Some exit)} body in
    [(entry, SKIP)] @ code1 @ [(new_label(), CJUMPF (t, exit))] @
      code2 @ [(new_label(), UJUMP entry)] @ [(exit, SKIP)]

and translate_for_stmt ctx target iter body = 
  let idx = "__tidx__" ^ new_temp_var() in 
  let titer = "__titer__" ^ new_temp_var() in 
  let length = "__tlen__" ^ new_temp_var() in
  let stmts = 
    (*
      idx = 0
      titer = iter
      length = len(iter)
      while idx < length:
        target = iter[idx]
        idx += 1
        body 
    *)
    [
      Assign ([Name idx], Constant (CInt 0)); 
      Assign ([Name titer], iter); 
      Assign ([Name length], Call (Name "len", [Name titer])); 
      While (
        Compare (Name idx, Lt, Name length), 
        [
          Assign ([target], Subscript (Name titer, Name idx)); 
          AugAssign (Name idx, Add, Constant (CInt 1))
        ] @ body
        )
    ] in 
    translate_stmts ctx stmts 

and is_the_isinstance_function expr = 
  match expr with
  | Name "isinstance" -> true 
  | _ -> false
  
and is_the_print_function expr = 
  match expr with
  | Name "print" -> true 
  | _ -> false

and is_the_len_function expr = 
  match expr with
  | Name "len" -> true 
  | _ -> false

and is_the_input_function expr = 
  match expr with
  | Name "input" -> true 
  | _ -> false

and is_the_int_function expr = 
  match expr with
  | Name "int" -> true 
  | _ -> false

and is_the_range_function expr = 
  match expr with
  | Name "range" -> true 
  | _ -> false

and is_the_append_function expr = 
  match expr with
  | Attribute (_, attr) when attr = "append" -> true 
  | _ -> false

and get_attribute_value expr = 
  match expr with
  | Attribute (e, _) -> e
  | _ -> raise (Failure "Cannot happen")
