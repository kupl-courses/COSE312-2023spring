open Ast 
open Lib.Util

let string_of_identifier id = id 

let rec tab indent = 
  if indent <= 0 then ""
  else "    " ^ tab (indent-1)

let rec string_of_module modul = 
  match modul with
  | Module a -> string_of_stmts 0 a.body
  | _ -> ""

and string_of_stmts indent stmts =  
  list_fold (fun stmt str -> str ^ string_of_stmt indent stmt ^ "\n") stmts ""

and string_of_stmt indent stmt : string = 
  match stmt with
  | FunctionDef a -> string_of_functiondef_stmt indent a.name a.args a.body a.returns a.decorator_list false 
  | AsyncFunctionDef a -> string_of_functiondef_stmt indent a.name a.args a.body a.returns a.decorator_list true
  | ClassDef a -> string_of_classdef_stmt indent a.name a.bases a.body a.decorator_list
  | Return a -> string_of_return_stmt indent a.value
  | Delete a -> string_of_delete_stmt indent a.targets 
  | Assign a -> string_of_assign_stmt indent a.targets a.value
  | AugAssign a -> string_of_augassign_stmt indent a.target a.op a.value 
  | AnnAssign a -> string_of_annassign_stmt indent a.target a.annotation a.value 
  | Expr a -> string_of_expr_stmt indent a.value 
  | For a -> string_of_for_stmt indent a.target a.iter a.body a.orelse false
  | AsyncFor a -> string_of_for_stmt indent a.target a.iter a.body a.orelse true
  | While a -> string_of_while_stmt indent a.test a.body a.orelse 
  | If a -> string_of_if_stmt indent a.test a.body a.orelse 
  | With a -> string_of_with_stmt indent a.items a.body false 
  | AsyncWith a -> string_of_with_stmt indent a.items a.body true 
  | Match a -> string_of_match_stmt indent a.subject a.cases 
  | Raise a -> string_of_raise_stmt indent a.exc a.cause 
  | Try a -> string_of_try_stmt indent a.body a.handlers a.orelse a.finalbody 
  | Assert a -> string_of_assert_stmt indent a.test a.msg 
  | Import a -> string_of_import_stmt indent a.names 
  | ImportFrom a -> string_of_importfrom_stmt indent a.modul a.names a.level 
  | Global a -> string_of_global_stmt indent a.names 
  | Nonlocal a -> string_of_nonlocal_stmt indent a.names 
  | Pass _ -> string_of_pass_stmt indent 
  | Break _ -> string_of_break_stmt indent 
  | Continue _ -> string_of_continue_stmt indent 

and string_of_functiondef_stmt indent name args body returns decorator_list is_async = 
  string_of_list (fun e -> tab indent ^ "@" ^ string_of_expr e ^ "\n") decorator_list ~first:"" ~last:"" ~sep:"" ^ 
  tab indent ^ (if is_async then "async " else "") ^ "def " ^ 
    string_of_identifier name ^ "(" ^ string_of_arguments args ^ ")" ^  
    begin 
      match returns with
      | None -> ""
      | Some s -> " -> " ^ string_of_expr s 
    end ^ ":\n" ^
  string_of_stmts (indent+1) body

and string_of_classdef_stmt indent name bases body decorator_list = 
  string_of_list (fun e -> tab indent ^ "@" ^ string_of_expr e ^ "\n") decorator_list ~first:"" ~last:"" ~sep:"" ^ 
  tab indent ^ "class " ^ string_of_identifier name ^ 
    (match bases with
    | [] -> "" 
    | _ -> string_of_list string_of_expr bases) ^ ":\n" ^
  string_of_stmts (indent+1) body 

and string_of_return_stmt indent value = 
  tab indent ^ "return " ^ 
    begin 
      match value with
      | None -> ""
      | Some e -> string_of_expr e 
    end 

and string_of_delete_stmt indent targets = 
  tab indent ^ "del " ^ string_of_list string_of_expr targets ~first:"" ~last:"" ~sep:", " 

and string_of_assign_stmt indent targets value = 
  tab indent ^ string_of_list string_of_expr targets ~first:"" ~last:"" ~sep:" = " ^ " = " ^ string_of_expr value 

and string_of_augassign_stmt indent target op value = 
  tab indent ^ string_of_expr target ^ " " ^ string_of_operator op ^ "= " ^ string_of_expr value 

and string_of_annassign_stmt indent target annot value = 
  tab indent ^ string_of_expr target ^ " : " ^ string_of_expr annot ^ 
    begin 
      match value with 
      | None -> ""
      | Some value -> " = " ^ string_of_expr value ^ "\n"
    end 

and string_of_for_stmt indent target iter body orelse is_async = 
  tab indent ^ (if is_async then "async " else "") ^ "for " ^ string_of_expr target ^ " in " ^ string_of_expr iter ^ ":\n" ^
    string_of_stmts (indent+1) body ^ 
  begin 
    match orelse with
    | [] -> ""
    | _ -> tab indent ^ "else:" ^ "\n" ^ string_of_stmts (indent+1) orelse 
  end

and string_of_while_stmt indent test body orelse =
  tab indent ^ "while " ^ string_of_expr test ^ ":\n" ^ 
    string_of_stmts (indent+1) body ^ 
  begin 
    match orelse with 
    | [] -> ""
    | _ -> tab indent ^ "else:\n" ^ string_of_stmts (indent+1) orelse 
  end

and string_of_if_stmt indent test body orelse = 
  tab indent ^ "if " ^ string_of_expr test ^ ":" ^ "\n" ^ 
    string_of_stmts (indent+1) body ^  
  begin
    match orelse with
    | [] -> ""
    | _ -> tab indent ^ "else: \n" ^ string_of_stmts (indent+1) orelse 
  end

and string_of_with_stmt indent items body is_async = 
  tab indent ^ (if is_async then "async " else "") ^ "with " ^ string_of_list string_of_withitem items ~first:"" ~last:"" ~sep:", " ^ ":\n" ^ 
    string_of_stmts (indent+1) body 

and string_of_match_stmt indent subject cases = 
  tab indent ^ "match " ^ string_of_expr subject ^ ":\n" ^ 
    string_of_list (fun (Match_case case) -> 
      string_of_matchcase (indent+1) case.pattern case.guard case.body  
    ) cases ~first:"" ~last:"" ~sep:""

and string_of_raise_stmt indent exc cause = 
  tab indent ^ 
  if exc = None && cause = None then "raise"
  else 
    begin 
      match exc with
      | None -> ""
      | Some e -> "raise "  ^ string_of_expr e 
    end ^ 
    begin 
      match cause with 
      | None -> ""
      | Some e -> " from " ^ string_of_expr e 
    end 

and string_of_try_stmt indent body handlers orelse finalbody = 
  tab indent ^ "try:\n" ^ 
    string_of_stmts (indent+1) body ^
  string_of_list (fun handler -> 
    string_of_excepthandler indent handler
  ) handlers ~first:"" ~last:"" ~sep:"" ^ 
  begin 
    if orelse = [] then ""
    else tab indent ^ "else:\n" ^ string_of_stmts (indent+1) orelse 
  end ^ 
  begin 
    if finalbody = [] then ""
    else tab indent ^ "finally:\n" ^ string_of_stmts (indent+1) finalbody 
  end 

and string_of_assert_stmt indent test msg =
  tab indent ^ "assert " ^ string_of_expr test ^ 
  begin 
    match msg with 
    | None -> ""
    | Some msg ->  ", " ^ string_of_expr msg 
  end 

and string_of_import_stmt indent names = 
  tab indent ^ "import " ^ string_of_list string_of_alias names ~first:"" ~last:""

and string_of_importfrom_stmt indent modul names _ (* level *) = 
  tab indent ^ (match modul with None -> "" | Some m -> "from " ^ m ^ " ") ^ 
    "import " ^ string_of_list string_of_alias names ~first:"" ~last:"" ~sep:", "

and string_of_global_stmt indent names = 
  tab indent ^ "global " ^ string_of_list (fun x -> x) names ~first:"" ~last:"" 

and string_of_nonlocal_stmt indent names = 
  tab indent ^ "nonlocal " ^ string_of_list (fun x -> x) names ~first:"" ~last:"" 

and string_of_expr_stmt indent value = tab indent ^ string_of_expr value 

and string_of_pass_stmt indent = tab indent ^ "pass" 

and string_of_break_stmt indent = tab indent ^ "break" 

and string_of_continue_stmt indent = tab indent ^ "continue" 

and string_of_expr ?(paren=true) expr  = 
  match expr with
  | BoolOp a -> string_of_boolop_expr a.op a.values 
  | NamedExpr a -> string_of_namedexpr_expr a.target a.value 
  | BinOp a -> string_of_binop_expr a.left a.op a.right 
  | UnaryOp a -> string_of_unaryop_expr a.op a.operand 
  | Lambda a -> string_of_lambda_expr a.args a.body 
  | IfExp a -> string_of_ifexp_expr a.test a.body a.orelse 
  | Dict a -> string_of_dict_expr a.keys a.values 
  | Set a -> string_of_set_expr a.elts 
  | ListComp a -> string_of_listcomp_expr a.elt a.generators 
  | SetComp a -> string_of_setcomp_expr a.elt a.generators 
  | DictComp a -> string_of_dictcomp_expr a.key a.value a.generators 
  | GeneratorExp a -> string_of_genexp_expr a.elt a.generators 
  | Await a -> string_of_await_expr a.value 
  | Yield a -> string_of_yield_expr a.value  
  | YieldFrom a -> string_of_yieldfrom_expr a.value  
  | Compare a -> string_of_compare_expr a.left a.ops a.comparators
  | Call a -> string_of_call_expr a.func a.args a.keywords 
  | FormattedValue a -> string_of_formattedvalue_expr a.value a.conversion a.format_spec
  | JoinedStr a -> string_of_joinedstr_expr a.values 
  | Constant a -> string_of_constant_expr a.value
  | Attribute a -> string_of_attribute_expr a.value a.attr 
  | Starred a -> string_of_starred_expr a.value 
  | Subscript a -> string_of_subscript_expr a.value a.slice 
  | Name a -> string_of_name_expr a.id 
  | List a -> string_of_list_expr a.elts 
  | Tuple a -> string_of_tuple_expr a.elts paren 
  | Slice a -> string_of_slice_expr a.lower a.upper a.step 

and string_of_boolop_expr op values = 
  string_of_list string_of_expr values 
    ~first:"(" ~last:")" ~sep:(" " ^ string_of_boolop op ^ " ")

and string_of_namedexpr_expr target value = 
  "(" ^ string_of_expr target ^ ":=" ^ string_of_expr value ^ ")"

and string_of_binop_expr left op right =
  "(" ^ string_of_expr left ^ " " ^ string_of_operator op ^ " " ^ string_of_expr right ^ ")"

and string_of_unaryop_expr op operand = 
  "(" ^ string_of_unaryop op ^ " " ^ string_of_expr operand ^ ")"

and string_of_lambda_expr args body = 
  "(" ^ "lambda " ^ string_of_arguments args ^ ": " ^ string_of_expr body ^ ")"

and string_of_ifexp_expr test body orelse = 
  "(" ^ string_of_expr body ^ " if " ^ string_of_expr test ^ " else " ^ string_of_expr orelse ^ ")"

and string_of_dict_expr keys values = 
  string_of_list (fun (k,v) -> 
    match k with
    | None -> "**" ^ string_of_expr v
    | Some k -> string_of_expr k ^ ":" ^ string_of_expr v) (List.combine keys values)
  ~first:"{" ~last:"}" ~sep:","

and string_of_set_expr elts = 
  string_of_list (fun e -> string_of_expr e 
  ) elts ~first:"{" ~last:"}" ~sep:","

and string_of_listcomp_expr elt generators = 
  "[" ^ string_of_expr elt ^ " " ^ 
  string_of_list string_of_comprehension generators ~first:"" ~last:"" ~sep:" " ^
  "]"

and string_of_setcomp_expr elt generators = 
  "{" ^ string_of_expr elt ^ " " ^ 
  string_of_list string_of_comprehension generators ~first:"" ~last:"" ~sep:" " ^
  "}"

and string_of_dictcomp_expr key value generators = 
  "{" ^ string_of_expr key ^ ":" ^ string_of_expr value ^ " " ^ 
  string_of_list string_of_comprehension generators ~first:"" ~last:"" ~sep:" " ^
  "}"

and string_of_genexp_expr elt generators = 
  "(" ^ string_of_expr elt ^ " " ^ string_of_list string_of_comprehension generators ~first:"" ~last:"" ^ ")"

and string_of_await_expr value = "(await " ^ string_of_expr value ^ ")"

and string_of_yield_expr value = 
  match value with
  | Some value -> "(yield " ^ string_of_expr value ^ ")"
  | None -> "yield"

and string_of_yieldfrom_expr value = "(yield from " ^ string_of_expr value ^ ")"

and string_of_compare_expr left ops comparators = 
  "(" ^ string_of_expr left ^ 
  string_of_list (fun (op, exp) -> string_of_cmpop op ^ " " ^ string_of_expr exp)
    (List.combine ops comparators) ~first:" " ~last:"" ~sep:"" ^ ")"

and string_of_call_expr func args keywords = 
  string_of_expr func ^ 
  "(" ^ 
    string_of_list string_of_expr args ~first:"" ~last:"" ~sep:", "^
    (if args = [] || keywords = [] then "" else ", ") ^
    string_of_list string_of_keyword keywords ~first:"" ~last:"" ~sep:", " ^ 
  ")"

and string_of_formattedvalue_expr value _ format_spec = 
  let format_spec_str = 
    match format_spec with 
    | None -> ""
    | Some s -> ":" ^ 
      begin 
        let str = string_of_expr s in
          if BatString.starts_with str "\"" && BatString.ends_with str "\"" 
          then BatString.sub str 1 (String.length str - 2) 
          else str
      end in 
  let value_str = string_of_expr value in 
  let value_str = BatString.replace_chars (function '"' -> "'" | c -> BatString.of_char c)  value_str in       
    "f\"" ^ "{" ^ value_str ^ format_spec_str ^  "}" ^ "\""

and string_of_joinedstr_expr values = 
  string_of_list string_of_expr values ~first:"" ~last:"" ~sep:"" (* TODO *)

and string_of_constant_expr c = 
  match c with
  | CInt n -> string_of_int n
  | CFloat f -> string_of_float f
  | CString s -> "\"" ^ String.escaped s ^ "\""
  | CBool b -> if b then "True" else "False" 
  | CNone -> "None"
  | CEllipsis -> "..."

and string_of_attribute_expr value attr = string_of_expr value ^ "." ^ string_of_identifier attr

and string_of_subscript_expr value slice =
  let is_simple_tuple slice = 
    match slice with 
    | Tuple a when a.elts <> [] -> List.for_all (function Starred _ -> false | _ -> true) a.elts 
    | _ -> false in 
  let not_simple_tuple = not (is_simple_tuple slice) in 
  let slice_str = string_of_expr slice ~paren:not_simple_tuple in 
    string_of_expr value ^ "[" ^ slice_str ^ "]"

and string_of_starred_expr value = "*" ^ string_of_expr value ^ ""

and string_of_name_expr id = id 

and string_of_list_expr elts = 
  "[" ^ string_of_list string_of_expr elts ~first:"" ~last:"" ^ "]"

and string_of_tuple_expr elts paren = 
  if paren then 
    let last = if List.length elts = 1 then ",)" else ")" in 
      string_of_list string_of_expr elts ~first:"(" ~last:last
  else 
    let last = if List.length elts = 1 then "," else "" in 
      string_of_list string_of_expr elts ~first:"" ~last:last

and string_of_slice_expr lower upper step = 
  let lower_str = match lower with None -> "" | Some lower -> string_of_expr lower in 
  let upper_str = match upper with None -> "" | Some upper -> string_of_expr upper in 
  let step_str = match step with None -> "" | Some step -> ":" ^ string_of_expr step in 
    lower_str ^ ":" ^ upper_str ^ step_str

and string_of_boolop op = 
  match op with
  | And -> "and"
  | Or -> "or"

and string_of_operator op = 
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | MatMult -> "*" (* CHECK *)
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "**"
  | LShift -> "<<"
  | RShift -> ">>"
  | BitOr -> "|"
  | BitXor -> "^"
  | BitAnd -> "&"
  | FloorDiv -> "//"

and string_of_unaryop op = 
  match op with
  | Invert -> "~"
  | Not -> "not"
  | UAdd -> "+"
  | USub -> "-"

and string_of_cmpop op = 
  match op with
  | Eq -> "=="
  | NotEq -> "!="
  | Lt -> "<"
  | LtE  -> "<="
  | Gt -> ">"
  | GtE -> ">="
  | Is -> "is"
  | IsNot -> "is not"
  | In -> "in"
  | NotIn -> "not in"

and string_of_comprehension (Comprehension a) = 
  "for " ^ string_of_expr a.target ^ " in " ^ string_of_expr a.iter ^ 
  if List.length a.ifs > 0 then string_of_list string_of_expr a.ifs ~first:" if " ~last:"" ~sep:" if "
  else ""

and string_of_excepthandler indent (ExceptHandler a) = 
  tab indent ^ "except " ^ 
  begin 
    match a.typ with 
    | None -> "" 
    | Some e -> "(" ^ string_of_expr e ^ ")"
  end ^ 
  begin 
    match a.name with 
    | None -> ":\n" 
    | Some id -> " as " ^ id ^ ":\n" 
  end ^ 
  string_of_stmts (indent+1) a.body  

and string_of_arguments (Arguments a) = 
  let args = List.rev (zip (List.rev a.args) (List.rev a.defaults)) in 
    string_of_list (fun (arg,default) -> 
      match arg, default with 
      | Some a, Some d -> string_of_arg a ^ "=" ^ string_of_expr d 
      | Some a, None -> string_of_arg a 
      | _ -> assert false
    ) args ~first:"" ~last:"" ~sep:", " ^ 
    (if a.args = [] || a.vararg = None then "" else ",") ^
    (match a.vararg with Some arg -> "*" ^ string_of_arg arg | _ -> "") ^ 
    (if (a.args = [] && a.vararg = None) || a.kwarg = None then "" else ",") ^ 
    (match a.kwarg with Some arg -> "**" ^ string_of_arg arg | _ -> "")

and string_of_arg (Arg a) = 
  a.arg ^ begin 
    match a.annotation with 
    | None -> ""
    | Some e -> " : " ^ string_of_expr e
  end

and string_of_keyword (Keyword a) = 
  match a.arg with 
  | Some arg -> arg ^ "=" ^ "(" ^ string_of_expr a.value ^ ")"
  | None -> "**" ^ string_of_expr a.value (* TODO *)

and string_of_alias (Alias a) = 
  a.name ^ (match a.asname with None -> "" | Some x -> " as " ^ x)

 and string_of_withitem (WithItem a) = 
  string_of_expr a.context_expr ^ 
  begin 
    match a.optional_vars with
    | Some e -> " as " ^ string_of_expr e 
    | None -> ""
  end 

and string_of_matchcase indent pattern _ body = 
  tab indent ^ "case " ^ string_of_pattern pattern ^ ":" ^ "\n" ^ 
    string_of_stmts (indent+1) body 

and string_of_pattern pattern = 
  match pattern with
  | MatchValue a -> string_of_expr a.value 
  | MatchSingleton a -> string_of_constant_expr a.value 
  | MatchSequence a -> string_of_list string_of_pattern a.patterns ~first:"[" ~last:"]" ~sep:", " 
  | MatchMapping a -> 
    "{" ^ 
    string_of_list (fun (k, p) -> 
      string_of_expr k ^ ":" ^ string_of_pattern p 
    ) (List.combine a.keys a.patterns) ~first:"" ~last:"" ~sep:"," ^ 
    begin 
      match a.rest with 
      | Some x -> "**" ^ x 
      | None -> ""
    end ^ 
    "}"
  | MatchClass a -> string_of_expr a.cls ^ string_of_list string_of_pattern a.patterns 
  | MatchStar a -> 
    begin 
      match a.name with
      | Some x -> "*" ^ x 
      | None -> "*_"
    end
  | MatchAs a -> 
    if a.pattern = None && a.name = None then "_" 
    else 
      begin 
        match a.pattern with 
        | None -> ""
        | Some p -> string_of_pattern p
      end  ^ 
      begin 
        match a.name with 
        | None -> ""
        | Some x -> (if a.pattern = None then "" else " as ") ^ x
      end
  | MatchOr a -> string_of_list string_of_pattern a.patterns ~first:"" ~last:"" ~sep:"|"