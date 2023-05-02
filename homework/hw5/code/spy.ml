type identifier = string
type constant = 
  | CInt of int
  | CString of string 
  | CBool of bool 
  | CNone 

type program = stmt list 

and stmt = 
  | FunctionDef of identifier * identifier list * stmt list
  | Return of expr option 
  | Assign of expr list * expr 
  | AugAssign of expr * operator * expr 
  | For of expr * expr * stmt list 
  | While of expr * stmt list 
  | If of expr * stmt list * stmt list 
  | Assert of expr 
  | Expr of expr 
  | Break 
  | Continue 
  | Pass 

and expr = 
  | BoolOp of boolop * expr list 
  | BinOp of expr * operator * expr 
  | UnaryOp of unaryop * expr 
  | IfExp of expr * expr * expr 
  | ListComp of expr * comprehension list 
  | Compare of expr * cmpop * expr 
  | Call of expr * expr list 
  | Constant of constant 
  | Attribute of expr * identifier 
  | Subscript of expr * expr 
  | Name of identifier 
  | List of expr list 
  | Tuple of expr list 
  | Lambda of identifier list * expr  

and boolop = And | Or

and comprehension = expr * expr * expr list 

and operator = Add | Sub | Mult | Div | Mod | Pow 

and unaryop = Not | UAdd | USub

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE 

(*********************************************************)
open Frontend

let null_attrs : Ast.attributes = {
  lineno = -1; 
  col_offset = -1;
  end_lineno = None;
  end_col_offset = None;
}

let spy2py_stmt stmt = 
  match stmt with 
  | FunctionDef _
  | Return _
  | Assign _
  | AugAssign _
  | For _
  | While _
  | If _
  | Assert _
  | Expr _
  | Break 
  | Pass 
  | Continue -> Ast.Continue { attrs=null_attrs } 
let spy2py_expr expr = 
  match expr with 
  | BoolOp _
  | BinOp _
  | UnaryOp _
  | IfExp _
  | ListComp _
  | Compare _
  | Call _
  | Constant _
  | Attribute _
  | Subscript _
  | Name _
  | List _
  | Lambda _ 
  | Tuple _ -> Ast.Constant { value=Ast.CInt 0; kind=None; attrs=null_attrs }

let string_of_stmt stmt = Frontend.Ast2string.string_of_stmt 0 (spy2py_stmt stmt)
let string_of_expr expr = Frontend.Ast2string.string_of_expr (spy2py_expr expr)