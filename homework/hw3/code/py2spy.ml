open Frontend
open Ast
open Ast2string

exception Unsupported of string 

let rec translate : Ast.modul -> Spy.program
=fun ast -> 
  match ast with
  | Module a -> trans_stmts a.body 
  | _ -> raise (Unsupported (string_of_module ast))

and trans_stmts : Ast.stmt list -> Spy.stmt list
=fun stmts -> List.map trans_stmt stmts 

and trans_stmt : Ast.stmt -> Spy.stmt 
=fun stmt -> 
  match stmt with 
  | FunctionDef a when is_simple_args a.args -> Spy.FunctionDef (a.name, trans_args a.args, trans_stmts a.body)
  | Return a -> Spy.Return (trans_expr_opt a.value)
  | Assign a -> Spy.Assign (trans_exprs a.targets, trans_expr a.value)
  | AugAssign a -> Spy.AugAssign (trans_expr a.target, trans_op a.op, trans_expr a.value)
  | For a when a.orelse == [] -> Spy.For (trans_expr a.target, trans_expr a.iter, trans_stmts a.body)
  | While a when a.orelse == [] -> Spy.While (trans_expr a.test, trans_stmts a.body)
  | If a -> Spy.If (trans_expr a.test, trans_stmts a.body, trans_stmts a.orelse)
  | Assert a -> Spy.Assert (trans_expr a.test)
  | Expr a -> Spy.Expr (trans_expr a.value)
  | Break _ -> Spy.Break 
  | Continue _ -> Spy.Continue 
  | Pass _ -> Spy.Pass
  | _ -> raise (Unsupported (string_of_stmt 0 stmt))

and is_simple_args (Arguments a) = 
  a.posonlyargs = [] && a.vararg = None && a.kwonlyargs = [] && a.kwarg = None 

and trans_args : Ast.arguments -> Spy.identifier list 
=fun (Arguments a) -> List.map (fun (Arg a) -> a.arg) a.args

and trans_exprs : Ast.expr list -> Spy.expr list 
=fun exprs -> List.map trans_expr exprs 

and trans_expr_opt : Ast.expr option -> Spy.expr option
=fun expr_opt -> 
  match expr_opt with 
  | None -> None 
  | Some expr -> Some (trans_expr expr)

and trans_expr : Ast.expr -> Spy.expr 
=fun expr -> 
  match expr with 
  | BoolOp a -> Spy.BoolOp (trans_boolop a.op, trans_exprs a.values)
  | BinOp a -> Spy.BinOp (trans_expr a.left, trans_op a.op, trans_expr a.right)
  | UnaryOp a -> Spy.UnaryOp (trans_uop a.op, trans_expr a.operand)
  | IfExp a -> Spy.IfExp (trans_expr a.test, trans_expr a.body, trans_expr a.orelse)
  | ListComp a -> Spy.ListComp (trans_expr a.elt, trans_comps a.generators)
  | Compare a when List.length a.ops = 1 -> Spy.Compare (trans_expr a.left, trans_cmpop (List.nth a.ops 0), trans_expr (List.nth a.comparators 0))
  | Call a when a.keywords = [] -> Spy.Call (trans_expr a.func, trans_exprs a.args)
  | Constant a -> Spy.Constant (trans_const a.value)
  | Attribute a -> Spy.Attribute (trans_expr a.value, a.attr) 
  | Subscript a -> Spy.Subscript (trans_expr a.value, trans_expr a.slice)
  | Name a -> Spy.Name (a.id) 
  | List a -> Spy.List (trans_exprs a.elts) 
  | Tuple a -> Spy.Tuple (trans_exprs a.elts)
  | Lambda a -> Spy.Lambda (trans_args a.args, trans_expr a.body)
  | _ -> raise (Unsupported (string_of_expr expr))
 
and trans_op : Ast.operator -> Spy.operator
=fun op -> 
  match op with
  | Add -> Spy.Add
  | Sub -> Spy.Sub 
  | Mult -> Spy.Mult 
  | FloorDiv -> Spy.Div
  | Mod -> Spy.Mod 
  | Pow -> Spy.Pow 
  | _ -> raise (Unsupported (string_of_operator op))

and trans_boolop : Ast.boolop -> Spy.boolop
=fun op -> 
  match op with 
  | And -> Spy.And 
  | Or -> Spy.Or

and trans_uop : Ast.unaryop -> Spy.unaryop
=fun op -> 
  match op with 
  | Not -> Spy.Not 
  | UAdd -> Spy.UAdd
  | USub -> Spy.USub
  | _ -> raise (Unsupported (string_of_unaryop op))

and trans_cmpop : Ast.cmpop -> Spy.cmpop
=fun op -> 
  match op with 
  | Eq -> Spy.Eq
  | NotEq -> Spy.NotEq
  | Lt -> Spy.Lt
  | LtE -> Spy.LtE
  | Gt -> Spy.Gt
  | GtE -> Spy.GtE
  | _ -> raise (Unsupported (string_of_cmpop op))

and trans_comps : Ast.comprehension list -> Spy.comprehension list 
=fun comps -> List.map trans_comp comps 

and trans_comp : Ast.comprehension -> Spy.comprehension 
=fun (Comprehension a) -> (trans_expr a.target, trans_expr a.iter, trans_exprs a.ifs)

and trans_const : Ast.constant -> Spy.constant
=fun const -> 
  match const with 
  | CInt n -> Spy.CInt n
  | CString s -> Spy.CString s 
  | CBool b -> Spy.CBool b
  | CNone -> Spy.CNone 
  | _ -> raise (Unsupported (string_of_constant_expr const))