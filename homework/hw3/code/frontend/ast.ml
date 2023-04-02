let python_version = "3.10.7" (* https://docs.python.org/3/library/ast.html *)

type identifier = string
type constant =  
  | CInt of int
  | CFloat of float 
  | CString of string 
  | CBool of bool 
  | CNone 
  | CEllipsis 

let ellipsis_str = "!!!..._encoding_of_ellipsis_as_string_...!!!"

type modul = 
  | Module of { body: stmt list; type_ignores: type_ignore list }
  | Interactive of { body: stmt list }
  | Expression of { body: expr }
  | FunctionType of { argtypes: expr list; returns: expr }

and t = modul

and attributes = { lineno: int; col_offset: int; end_lineno: int option; end_col_offset: int option}

and stmt = 
  | FunctionDef of { name: identifier;  args: arguments; body: stmt list; decorator_list: expr list; returns: expr option; type_comment: string option; attrs : attributes }
  | AsyncFunctionDef of { name: identifier;  args: arguments; body: stmt list; decorator_list: expr list; returns: expr option; type_comment: string option; attrs: attributes }  
  | ClassDef of { name : identifier; bases: expr list; keywords: keyword list; body: stmt list; decorator_list: expr list; attrs: attributes }
  | Return of { value: expr option; attrs: attributes }
  | Delete of { targets: expr list; attrs: attributes }
  | Assign of { targets: expr list; value: expr; type_comment: string option; attrs: attributes }
  | AugAssign of { target: expr; op: operator; value: expr; attrs: attributes }
  | AnnAssign of { target: expr; annotation: expr; value: expr option; simple: int; attrs: attributes }
  | For of { target: expr; iter: expr; body: stmt list; orelse: stmt list; type_comment: string option; attrs: attributes }
  | AsyncFor of { target: expr; iter: expr; body: stmt list; orelse: stmt list; type_comment: string option; attrs: attributes }
  | While of { test: expr; body: stmt list; orelse: stmt list; attrs: attributes }
  | If of { test: expr; body: stmt list; orelse: stmt list; attrs: attributes }
  | With of { items: withitem list; body: stmt list; type_comment: string option; attrs: attributes }
  | AsyncWith of { items: withitem list; body: stmt list; type_comment: string option; attrs: attributes }
  | Match of { subject: expr; cases: match_case list; attrs: attributes }
  | Raise of { exc: expr option; cause:  expr option; attrs: attributes }
  | Try of { body: stmt list; handlers: excepthandler list; orelse: stmt list; finalbody: stmt list; attrs: attributes }
  | Assert of { test: expr; msg: expr option; attrs: attributes }
  | Import of { names: alias list; attrs: attributes }
  | ImportFrom of { modul: identifier option; names: alias list; level: int option; attrs: attributes }
  | Global of { names: identifier list; attrs: attributes }
  | Nonlocal of { names: identifier list; attrs: attributes } 
  | Expr of { value: expr; attrs: attributes }
  | Pass of { attrs: attributes }
  | Break of { attrs: attributes }
  | Continue of { attrs: attributes }

and expr = 
  | BoolOp of { op: boolop; values: expr list; attrs: attributes }
  | NamedExpr of { target: expr; value:  expr; attrs: attributes }
  | BinOp of { left: expr; op: operator; right: expr; attrs: attributes }
  | UnaryOp of { op: unaryop; operand: expr; attrs: attributes }
  | Lambda of { args: arguments; body: expr; attrs: attributes }
  | IfExp of { test: expr; body:  expr; orelse: expr; attrs: attributes }
  | Dict of { keys: (expr option) list; (* key can be null, e.g., {a : "1", **d } *) values: expr list; attrs: attributes }
  | Set of { elts: expr list; attrs: attributes }
  | ListComp of { elt: expr; generators: comprehension list; attrs: attributes }
  | SetComp of { elt: expr; generators: comprehension list; attrs: attributes }
  | DictComp of { key: expr; value: expr; generators: comprehension list; attrs: attributes }
  | GeneratorExp of { elt: expr; generators: comprehension list; attrs: attributes }
  | Await of { value: expr; attrs: attributes }
  | Yield of { value: expr option; attrs: attributes }
  | YieldFrom of { value: expr; attrs: attributes }
  | Compare of { left: expr; ops: cmpop list; comparators: expr list; attrs: attributes }
  | Call of { func: expr; args: expr list; keywords: keyword list; attrs: attributes }
  | FormattedValue of { value: expr; conversion: int; format_spec: expr option; attrs: attributes }
  | JoinedStr of { values: expr list; attrs: attributes }
  | Constant of { value: constant; kind: string option; attrs: attributes }
  | Attribute of { value: expr; attr: identifier; ctx: expr_context; attrs: attributes }
  | Subscript of { value: expr; slice: expr; ctx: expr_context; attrs: attributes }
  | Starred of { value: expr; ctx: expr_context; attrs: attributes }
  | Name of { id: identifier; ctx: expr_context; attrs: attributes }
  | List of { elts: expr list; ctx: expr_context; attrs: attributes }
  | Tuple of { elts: expr list; ctx: expr_context; attrs: attributes }
  | Slice of { lower: expr option; upper: expr option; step: expr option; attrs: attributes }

and expr_context = Load | Store | Del 

and boolop = And | Or

and operator = Add | Sub | Mult | MatMult | Div | Mod | Pow | LShift
            | RShift | BitOr | BitXor | BitAnd | FloorDiv

and unaryop = Invert | Not | UAdd | USub

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

and comprehension = Comprehension of { target: expr; iter: expr; ifs: expr list; is_async: int }

and excepthandler = ExceptHandler of { typ: expr option; name: identifier option; body: stmt list; attrs: attributes }

and arguments = Arguments of { posonlyargs: arg list; args: arg list; vararg: arg option; kwonlyargs: arg list; kw_defaults: expr list; kwarg: arg option; defaults: expr list } 

and arg = Arg of { arg: identifier; annotation: expr option; type_comment: string option; attrs: attributes }

and keyword = Keyword of { arg: identifier option; value: expr; attrs: attributes }

and alias = Alias of { name: identifier; asname: identifier option; attrs: attributes }

and withitem = WithItem of { context_expr: expr; optional_vars: expr option }

and match_case = Match_case of { pattern: pattern; guard: expr option; body: stmt list }

and pattern = 
  | MatchValue of { value: expr; attrs: attributes }
  | MatchSingleton of { value: constant; attrs: attributes }
  | MatchSequence of { patterns: pattern list; attrs: attributes }
  | MatchMapping of { keys: expr list; patterns: pattern list; rest: identifier option; attrs: attributes }
  | MatchClass of { cls: expr; patterns: pattern list; kwd_attrs: identifier list; kwd_patterns: pattern list; attrs: attributes }
  | MatchStar of { name: identifier option; attrs: attributes }
  | MatchAs of { pattern: pattern option; name: identifier option; attrs: attributes }
  | MatchOr of { patterns: pattern list; attrs: attributes }

and type_ignore = TypeIgnore of { lineno: int; tag: string }
