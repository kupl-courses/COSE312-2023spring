open Core
open Ast
open Yojson.Basic.Util 

exception TranslationError of string

let null_attrs = {
  lineno = -1; 
  col_offset = -1;
  end_lineno = None;
  end_col_offset = None;
}

let null_expr = Set { elts=[]; attrs=null_attrs }

let get_attrs json = 
  {
    lineno = json |> member "lineno" |> to_int;
    col_offset = json |> member "col_offset" |> to_int;
    end_lineno = json |> member "end_lineno" |> to_option to_int;
    end_col_offset = json |> member "end_col_offset" |> to_option to_int
  }

let rec to_module json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Module" = 0) in
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let type_ignores = json |> member "type_ignores" |> to_list |> List.map ~f:to_type_ignore in 
    Module { body = body; type_ignores = type_ignores }

and to_stmt json = 
  let _type = json |> member "_type" |> to_string in 
    match _type with
    | "FunctionDef" -> to_functiondef_stmt json 
    | "AsyncFunctionDef" -> to_asyncfunctiondef_stmt json 
    | "ClassDef" -> to_classdef_stmt json
    | "Return" -> to_return_stmt json
    | "Delete" -> to_delete_stmt json 
    | "Assign" -> to_assign_stmt json 
    | "AugAssign" -> to_augassign_stmt json
    | "AnnAssign" -> to_annassign_stmt json
    | "For" -> to_for_stmt json 
    | "AsyncFor" -> to_asyncfor_stmt json 
    | "While" -> to_while_stmt json 
    | "If" -> to_if_stmt json 
    | "With" -> to_with_stmt json 
    | "AsyncWith" -> to_asyncwith_stmt json 
    | "Match" -> to_match_stmt json 
    | "Raise" -> to_raise_stmt json 
    | "Try" -> to_try_stmt json 
    | "Assert" -> to_assert_stmt json 
    | "Import" -> to_import_stmt json
    | "ImportFrom" -> to_importfrom_stmt json 
    | "Global" -> to_global_stmt json 
    | "Nonlocal" -> to_nonlocal_stmt json 
    | "Expr" -> to_expr_stmt json 
    | "Pass" -> to_pass_stmt json 
    | "Break" -> to_break_stmt json 
    | "Continue" -> to_continue_stmt json 
    | _ -> raise (TranslationError ("to_stmt: " ^ _type))

and to_try_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Try" = 0) in
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let handlers = json |> member "handlers" |> to_list |> List.map ~f:to_excepthandler in 
  let orelse = json |> member "orelse" |> to_list |> List.map ~f:to_stmt in 
  let finalbody = json |> member "finalbody" |> to_list |> List.map ~f:to_stmt in 
  let attrs = get_attrs json in 
    Try { body=body; handlers=handlers; orelse=orelse; finalbody=finalbody; attrs=attrs }

and to_with_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "With" = 0) in
  let items = json |> member "items" |> to_list |> List.map ~f:to_withitem in 
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let type_comment = json |> member "type_comment" |> to_string_option in 
  let attrs = get_attrs json in 
    With { items=items; body=body; type_comment=type_comment; attrs=attrs }

and to_asyncwith_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "AsyncWith" = 0) in
  let items = json |> member "items" |> to_list |> List.map ~f:to_withitem in 
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let type_comment = json |> member "type_comment" |> to_string_option in 
  let attrs = get_attrs json in 
    AsyncWith { items=items; body=body; type_comment=type_comment; attrs=attrs }

and to_match_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Match" = 0) in
  let subject = json |> member "subject" |> to_expr in 
  let cases = json |> member "cases" |> to_list |> List.map ~f:to_matchcase in 
  let attrs = get_attrs json in 
    Match { subject=subject; cases=cases; attrs=attrs }

and to_global_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Global" = 0) in
  let ids = json |> member "names" |> to_list |> List.map ~f:to_string in 
  let attrs = get_attrs json in 
    Global { names=ids; attrs=attrs }

and to_nonlocal_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Nonlocal" = 0) in
  let ids = json |> member "names" |> to_list |> List.map ~f:to_string in 
  let attrs = get_attrs json in 
    Nonlocal { names=ids; attrs=attrs }

and to_raise_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Raise" = 0) in
  let exc = json |> member "exc" |> to_option to_expr in 
  let cause = json |> member "cause" |> to_option to_expr in 
  let attrs = get_attrs json in 
    Raise { exc=exc; cause=cause; attrs=attrs }

and to_assert_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Assert" = 0) in
  let test = json |> member "test" |> to_expr in 
  let msg = json |> member "msg" |> to_option to_expr in 
  let attrs = get_attrs json in 
    Assert { test=test; msg=msg; attrs=attrs }

and to_if_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "If" = 0) in
  let test = json |> member "test" |> to_expr in
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let orelse = json |> member "orelse" |> to_list |> List.map ~f:to_stmt in   
  let attrs = get_attrs json in 
    If { test=test; body=body; orelse=orelse; attrs=attrs }

and to_import_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Import" = 0) in
  let alias = json |> member "names" |> to_list |> List.map ~f:to_alias in 
  let attrs = get_attrs json in 
    Import { names=alias; attrs=attrs }

and to_importfrom_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "ImportFrom" = 0) in
  let module_id = json |> member "module" |> to_string_option in 
  let alias = json |> member "names" |> to_list |> List.map ~f:to_alias in 
  let level = json |> member "level" |> to_int_option in 
  let attrs = get_attrs json in 
    ImportFrom { modul=module_id; names=alias; level=level; attrs=attrs }

and to_while_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "While" = 0) in
  let test = json |> member "test" |> to_expr in 
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let orelse = json |> member "orelse" |> to_list |> List.map ~f:to_stmt in 
  let attrs = get_attrs json in 
    While { test=test; body=body; orelse=orelse; attrs=attrs }

and to_for_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "For" = 0) in
  let target = json |> member "target" |> to_expr in 
  let iter = json |> member "iter" |> to_expr in 
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let orelse = json |> member "orelse" |> to_list |> List.map ~f:to_stmt in 
  let type_comment = json |> member "type_comment" |> to_option to_string in
  let attrs = get_attrs json in 
    For { target=target; iter=iter; body=body; orelse=orelse; type_comment=type_comment; attrs=attrs }

and to_asyncfor_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "AsyncFor" = 0) in
  let target = json |> member "target" |> to_expr in 
  let iter = json |> member "iter" |> to_expr in 
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let orelse = json |> member "orelse" |> to_list |> List.map ~f:to_stmt in 
  let type_comment = json |> member "type_comment" |> to_option to_string in
  let attrs = get_attrs json in 
    AsyncFor { target=target; iter=iter; body=body; orelse=orelse; type_comment=type_comment; attrs=attrs }

and to_assign_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Assign" = 0) in
  let targets = json |> member "targets" |> to_list |> List.map ~f:to_expr in 
  let value = json |> member "value" |> to_expr in 
  let type_comment = json |> member "type_comment" |> to_option to_string in
  let attrs = get_attrs json in 
    Assign { targets=targets; value=value; type_comment=type_comment; attrs=attrs }

and to_annassign_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "AnnAssign" = 0) in
  let target = json |> member "target" |> to_expr in 
  let annotation = json |> member "annotation" |> to_expr in 
  let value = json |> member "value" |> to_option to_expr in 
  let simple = json |> member "simple" |> to_int in 
  let attrs = get_attrs json in 
    AnnAssign { target=target; annotation=annotation; value=value; simple=simple; attrs=attrs }

and to_augassign_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "AugAssign" = 0) in
  let target = json |> member "target" |> to_expr in 
  let op = json |> member "op" |> to_operator in 
  let value = json |> member "value" |> to_expr in 
  let attrs = get_attrs json in 
    AugAssign { target=target; op=op; value=value; attrs=attrs }

and to_classdef_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "ClassDef" = 0) in
  let name = json |> member "name" |> to_string in 
  let bases = json |> member "bases" |> to_list |> List.map ~f:to_expr in 
  let keywords = json |> member "keywords" |> to_list |> List.map ~f:to_keyword in 
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let decos = json |> member "decorator_list" |> to_list |> List.map ~f:to_expr in
  let attrs = get_attrs json in 
    ClassDef { name=name; bases=bases; keywords=keywords; body=body; decorator_list=decos; attrs=attrs }

and to_asyncfunctiondef_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "AsyncFunctionDef" = 0) in
  let name = json |> member "name" |> to_string in 
  let args = json |> member "args" |> to_args in
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let decos = json |> member "decorator_list" |> to_list |> List.map ~f:to_expr in 
  let attrs = get_attrs json in 
  let returns = json |> member "returns" |> to_option (fun j -> to_expr j) in 
  let type_comment = json |> member "type_comment" |> to_option to_string in 
    AsyncFunctionDef { name=name;args=args; body=body; decorator_list=decos; returns=returns; type_comment=type_comment; attrs=attrs }

and to_functiondef_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "FunctionDef" = 0) in
  let name = json |> member "name" |> to_string in 
  let args = json |> member "args" |> to_args in
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let decos = json |> member "decorator_list" |> to_list |> List.map ~f:to_expr in 
  let attrs = get_attrs json in 
  let returns = json |> member "returns" |> to_option (fun j -> to_expr j) in 
  let type_comment = json |> member "type_comment" |> to_option to_string in 
    FunctionDef { name=name;args=args; body=body; decorator_list=decos; returns=returns; type_comment=type_comment; attrs=attrs }

and to_expr_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Expr" = 0) in
  let attrs = get_attrs json in 
  let value = json |> member "value" |> to_expr in
    Expr { value=value; attrs=attrs }

and to_return_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Return" = 0) in
  let attrs = get_attrs json in 
  let value = json |> member "value" |> to_option (fun j -> to_expr j) in
    Return { value=value; attrs=attrs }

and to_delete_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Delete" = 0) in
  let attrs = get_attrs json in 
  let targets = json |> member "targets" |> to_list |> List.map ~f:to_expr in
    Delete { targets=targets; attrs=attrs }

and to_pass_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Pass" = 0) in
  let attrs = get_attrs json in 
    Pass { attrs=attrs }

and to_continue_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Continue" = 0) in
  let attrs = get_attrs json in 
    Continue { attrs=attrs }

and to_break_stmt json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Break" = 0) in
  let attrs = get_attrs json in 
    Break { attrs=attrs }

and to_expr json = 
  let _type = json |> member "_type" |> to_string in 
    match _type with
    | "BoolOp" -> to_boolop_expr json  
    | "NamedExpr" -> to_namedexpr_expr json 
    | "BinOp" -> to_binop_expr json  
    | "UnaryOp" -> to_unaryop_expr json 
    | "Lambda" -> to_lambda_expr json  
    | "IfExp" -> to_ifexp_expr json 
    | "Dict" -> to_dict_expr json 
    | "DictComp" -> to_dictcomp_expr json 
    | "Set" -> to_set_expr json 
    | "SetComp" -> to_setcomp_expr json 
    | "ListComp" -> to_listcomp_expr json 
    | "GeneratorExp" -> to_generatorexp_expr json  
    | "Await" -> to_await_expr json  
    | "Yield" -> to_yield_expr json 
    | "YieldFrom" -> to_yieldfrom_expr json  
    | "Compare" -> to_compare_expr json 
    | "Call" ->  to_call_expr json
    | "FormattedValue" -> to_formattedvalue_expr json
    | "JoinedStr" -> to_joinedstr_expr json 
    | "Constant" -> to_constant_expr json 
    | "Attribute" -> to_attr_expr json 
    | "Subscript" -> to_subscript_expr json 
    | "Starred" -> to_starred_expr json 
    | "Name" -> to_name_expr json         
    | "List" -> to_list_expr json 
    | "Tuple" -> to_tuple_expr json 
    | "Slice" -> to_slice_expr json 
    | _ -> raise (TranslationError ("to_expr: " ^ _type))

and to_starred_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Starred" = 0) in
  let value = json |> member "value" |> to_expr in 
  let ctx = json |> member "ctx" |> to_expr_context in 
  let attrs = get_attrs json in 
    Starred { value=value; ctx=ctx; attrs=attrs }

and to_yield_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Yield" = 0) in
  let value = json |> member "value" |> to_option to_expr in 
  let attrs = get_attrs json in 
    Yield { value=value; attrs=attrs }

and to_yieldfrom_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "YieldFrom" = 0) in
  let value = json |> member "value" |> to_expr in 
  let attrs = get_attrs json in 
    YieldFrom { value=value; attrs=attrs }

and to_await_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Await" = 0) in
  let value = json |> member "value" |> to_expr in 
  let attrs = get_attrs json in 
    Await { value=value; attrs=attrs }

and to_generatorexp_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "GeneratorExp" = 0) in
  let elt = json |> member "elt" |> to_expr in 
  let generators = json |> member "generators" |> to_list |> List.map ~f:to_comprehension in 
  let attrs = get_attrs json in 
    GeneratorExp { elt=elt; generators=generators; attrs=attrs }

and to_lambda_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Lambda" = 0) in
  let args = json |> member "args" |> to_args in 
  let body = json |> member "body" |> to_expr in 
  let attrs = get_attrs json in 
    Lambda { args=args; body=body; attrs=attrs }

and to_namedexpr_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "NamedExpr" = 0) in
  let target = json |> member "target" |> to_expr in 
  let value = json |> member "value" |> to_expr in 
  let attrs = get_attrs json in 
    NamedExpr { target=target; value=value; attrs=attrs }
  
and to_slice_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Slice" = 0) in
  let lower = json |> member "lower" |> to_option to_expr in 
  let upper = json |> member "upper" |> to_option to_expr in 
  let step = json |> member "step" |> to_option to_expr in 
  let attrs = get_attrs json in 
    Slice { lower=lower; upper=upper; step=step; attrs=attrs }

and to_ifexp_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "IfExp" = 0) in
  let test = json |> member "test" |> to_expr in 
  let body = json |> member "body" |> to_expr in 
  let orelse = json |> member "orelse" |> to_expr in 
  let attrs = get_attrs json in 
    IfExp { test=test; body=body; orelse=orelse; attrs=attrs }

and to_formattedvalue_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "FormattedValue" = 0) in
  let value = json |> member "value" |> to_expr in 
  let conversion = json |> member "conversion" |> to_int in
  let format_spec = json |> member "format_spec" |> to_option to_expr in 
  let attrs = get_attrs json in 
    FormattedValue { value=value; conversion=conversion; format_spec=format_spec; attrs=attrs }  

and to_joinedstr_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "JoinedStr" = 0) in
  let values = json |> member "values" |> to_list |> List.map ~f:to_expr in 
  let attrs = get_attrs json in 
    JoinedStr { values=values; attrs=attrs }

and to_tuple_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Tuple" = 0) in
  let elts = json |> member "elts" |> to_list |> List.map ~f:to_expr in 
  let ctx = json |> member "ctx" |> to_expr_context in 
  let attrs = get_attrs json in 
    Tuple { elts=elts; ctx=ctx; attrs=attrs }

and to_dict_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Dict" = 0) in
  let keys = json |> member "keys" |> to_list |> List.map ~f:(to_option to_expr) in 
  let values = json |> member "values" |> to_list |> List.map ~f:to_expr in 
  let attrs = get_attrs json in 
    Dict { keys=keys; values=values; attrs=attrs }

and to_set_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Set" = 0) in
  let elts = json |> member "elts" |> to_list |> List.map ~f:to_expr in 
  let attrs = get_attrs json in 
    Set { elts=elts; attrs=attrs }

and to_list_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "List" = 0) in
  let elts = json |> member "elts" |> to_list |> List.map ~f:to_expr in 
  let ctx = json |> member "ctx" |> to_expr_context in 
  let attrs = get_attrs json in 
    List { elts=elts; ctx=ctx; attrs=attrs }

and to_setcomp_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "SetComp" = 0) in
  let elt = json |> member "elt" |> to_expr in 
  let generators = json |> member "generators" |> to_list |> List.map ~f:to_comprehension in 
  let attrs = get_attrs json in 
    SetComp { elt=elt; generators=generators; attrs=attrs }

and to_dictcomp_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "DictComp" = 0) in
  let key = json |> member "key" |> to_expr in 
  let value = json |> member "value" |> to_expr in 
  let generators = json |> member "generators" |> to_list |> List.map ~f:to_comprehension in 
  let attrs = get_attrs json in 
    DictComp { key=key; value=value; generators=generators; attrs=attrs }

and to_listcomp_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "ListComp" = 0) in
  let elt = json |> member "elt" |> to_expr in 
  let generators = json |> member "generators" |> to_list |> List.map ~f:to_comprehension in 
  let attrs = get_attrs json in 
    ListComp { elt=elt; generators=generators; attrs=attrs }

and to_compare_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Compare" = 0) in
  let left = json |> member "left" |> to_expr in 
  let ops = json |> member "ops" |> to_list |> List.map ~f:to_cmpop in 
  let comparators = json |> member "comparators" |> to_list |> List.map ~f:to_expr in 
  let attrs = get_attrs json in 
    Compare { left=left; ops=ops; comparators=comparators; attrs=attrs }

and to_call_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Call" = 0) in
  let func = json |> member "func" |> to_expr in 
  let args = json |> member "args" |> to_list |> List.map ~f:to_expr in 
  let keywords = json |> member "keywords" |> to_list |> List.map ~f:to_keyword in 
  let attrs = get_attrs json in 
    Call { func=func; args=args; keywords=keywords; attrs=attrs }

and to_boolop_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "BoolOp" = 0) in
  let op = json |> member "op" |> to_boolop in 
  let values = json |> member "values" |> to_list |> List.map ~f:to_expr in 
  let attrs = get_attrs json in 
    BoolOp { op=op; values=values; attrs=attrs }

and to_binop_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "BinOp" = 0) in
  let left = json |> member "left" |> to_expr in 
  let op = json |> member "op" |> to_operator in 
  let right = json |> member "right" |> to_expr in 
  let attrs = get_attrs json in 
    BinOp { left=left; op=op; right=right; attrs=attrs }  

and to_unaryop_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "UnaryOp" = 0) in
  let unaryop = json |> member "op" |> to_unaryop in 
  let operand = json |> member "operand" |> to_expr in 
  let attrs = get_attrs json in 
    UnaryOp { op=unaryop; operand=operand; attrs=attrs }

and to_attr_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Attribute" = 0) in
  let value = json |> member "value" |> to_expr in 
  let id = json |> member "attr" |> to_string in 
  let ctx = json |> member "ctx" |> to_expr_context in 
  let attrs = get_attrs json in 
    Attribute { value=value; attr=id; ctx=ctx; attrs=attrs }

and to_name_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Name" = 0) in
  let id = json |> member "id" |> to_string in 
  let ctx = json |> member "ctx" |> to_expr_context in 
  let attrs = get_attrs json in 
    Name { id=id; ctx=ctx; attrs=attrs }

and to_subscript_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Subscript" = 0) in
  let value = json |> member "value" |> to_expr in 
  let slice = json |> member "slice" |> to_expr in 
  let ctx = json |> member "ctx" |> to_expr_context in 
  let attrs = get_attrs json in 
    Subscript { value=value; slice=slice; ctx=ctx; attrs=attrs }

and to_constant_expr json = 
  let _type = json |> member "_type" |> to_string in 
  let _ = assert (String.compare _type "Constant" = 0) in
  let value = to_constant json in 
  let kind = json |> member "kind" |> to_option to_string in 
  let attrs = get_attrs json in 
    Constant { value=value; kind=kind; attrs=attrs }

and to_comprehension json = 
  let target = json |> member "target" |> to_expr in 
  let iter = json |> member "iter" |> to_expr in 
  let ifs = json |> member "ifs" |> to_list |> List.map ~f:to_expr in 
  let is_async = json |> member "is_async" |> to_int in 
    Comprehension { target=target; iter=iter; ifs=ifs; is_async=is_async }

and to_expr_context json = 
  let _type = json |> member "_type" |> to_string in 
    match _type with
    | "Store" -> Store
    | "Load" -> Load
    | "Del" -> Del
    | _ -> raise (TranslationError "to_expr_context")

and to_alias json = 
  let name = json |> member "name" |> to_string in 
  let asname = json |> member "asname" |> to_string_option in 
  let attrs = get_attrs json in
    Alias { name=name; asname=asname; attrs=attrs }

and to_keyword json = 
  let arg = json |> member "arg" |> to_option to_string in
  let value = json |> member "value" |> to_expr in 
  let attrs = get_attrs json in 
    Keyword { arg=arg; value=value; attrs=attrs }

and to_boolop json = 
  let _type = json |> member "_type" |> to_string in 
    match _type with
    | "And" -> And
    | "Or" -> Or
    | _ -> raise (TranslationError ("to_boolop: " ^ _type))

and to_operator json = 
  let _type = json |> member "_type" |> to_string in 
    match _type with
    | "Add" -> Add 
    | "Sub" -> Sub 
    | "Mult" -> Mult 
    | "MatMult" -> MatMult 
    | "Div" -> Div 
    | "Mod" -> Mod 
    | "Pow" -> Pow 
    | "LShift" -> LShift        
    | "RShift" -> RShift 
    | "BitOr" -> BitOr 
    | "BitXor" -> BitXor 
    | "BitAnd" -> BitAnd 
    | "FloorDiv" -> FloorDiv
    | _ -> raise (TranslationError ("to_operator: " ^ _type))

and to_unaryop json = 
  let _type = json |> member "_type" |> to_string in 
    match _type with
    | "Invert" -> Invert 
    | "Not" -> Not
    | "UAdd" -> UAdd
    | "USub" -> USub
    | _ -> raise (TranslationError ("to_unaryop: " ^ _type))

and to_cmpop json = 
  let _type = json |> member "_type" |> to_string in 
    match _type with
    | "Eq" -> Eq 
    | "NotEq" -> NotEq
    | "Lt" -> Lt
    | "LtE" -> LtE
    | "Gt" -> Gt
    | "GtE" -> GtE
    | "Is" -> Is
    | "IsNot" -> IsNot
    | "In" -> In
    | "NotIn" -> NotIn
    | _ -> raise (TranslationError ("to_cmpop: " ^ _type))

and to_withitem json = 
  let ctx_expr = json |> member "context_expr" |> to_expr in 
  let opt_vars = json |> member "optional_vars" |> to_option to_expr in 
    WithItem { context_expr=ctx_expr; optional_vars=opt_vars }

and to_excepthandler json = 
  let typ = json |> member "type" |> to_option to_expr in 
  let id = json |> member "name" |> to_option to_string in
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
  let attrs = get_attrs json in 
    ExceptHandler { typ=typ; name=id; body=body; attrs=attrs }

and to_matchcase json = 
  let pattern = json |> member "pattern" |> to_pattern in 
  let guard = json |> member "guard" |> to_option to_expr in 
  let body = json |> member "body" |> to_list |> List.map ~f:to_stmt in 
    Match_case { pattern=pattern; guard=guard; body=body }

and to_pattern json = 
  let _type = json |> member "_type" |> to_string in
  let attrs = get_attrs json in 
    match _type with
    | "MatchValue" -> 
      let value = json |> member "value" |> to_expr in 
        MatchValue { value=value; attrs=attrs }
    | "MatchSingleton" -> 
      let value = json |> member "value" |> to_constant in 
        MatchSingleton { value=value; attrs=attrs }
    | "MatchSequence" -> 
      let patterns = json |> member "patterns" |> to_list |> List.map ~f:to_pattern in 
        MatchSequence { patterns=patterns; attrs=attrs }
    | "MatchMapping" -> 
      let keys = json |> member "keys" |> to_list |> List.map ~f:to_expr in 
      let patterns = json |> member "patterns" |> to_list |> List.map ~f:to_pattern in 
      let rest = json |> member "rest" |> to_option to_string in 
        MatchMapping { keys=keys; patterns=patterns; rest=rest; attrs=attrs }
    | "MatchClass" -> 
      let cls = json |> member "cls" |> to_expr in 
      let patterns = json |> member "patterns" |> to_list |> List.map ~f:to_pattern in 
      let kwd_attrs = json |> member "kwd_attrs" |> to_list |> List.map ~f:to_string in 
      let kwd_patterns = json |> member "kwd_patterns" |> to_list |> List.map ~f:to_pattern in 
        MatchClass { cls=cls; patterns=patterns; kwd_attrs=kwd_attrs; kwd_patterns=kwd_patterns; attrs=attrs }
    | "MatchStar" -> 
      let name = json |> member "name" |> to_option to_string in 
        MatchStar { name=name; attrs=attrs }
    | "MatchAs" -> 
      let pattern = json |> member "pattern" |> to_option to_pattern in 
      let name = json |> member "name" |> to_option to_string in 
        MatchAs { pattern=pattern; name=name; attrs=attrs }
    | "MatchOr" -> 
      let patterns = json |> member "patterns" |> to_list |> List.map ~f:to_pattern in 
        MatchOr { patterns=patterns; attrs=attrs }
    | _ -> raise (TranslationError "to_pattern: other cases should not occur")

and to_args json = 
  let posonlyargs = json |> member "posonlyargs" |> to_list |> List.map ~f:to_arg in 
  let args = json |> member "args" |> to_list |> List.map ~f:to_arg in 
  let vararg = json |> member "vararg" |> to_option to_arg in 
  let kwonlargs = json |> member "kwonlyargs" |> to_list |> List.map ~f:to_arg in 
  let kw_defaults = json |> member "kw_defaults" |> to_list |> List.map ~f:to_expr in 
  let kwarg = json |> member "kwarg" |> to_option to_arg in 
  let defaults = json |> member "defaults" |> to_list |> List.map ~f:to_expr in 
    Arguments { posonlyargs=posonlyargs; args=args; vararg=vararg; kwonlyargs=kwonlargs; kw_defaults=kw_defaults; kwarg=kwarg; defaults=defaults }

and to_arg json = 
  let arg = json |> member "arg" |> to_string in 
  let annot = json |> member "annotation" |> to_option to_expr in 
  let type_comment = json |> member "type_comment" |> to_option to_string in 
  let attrs = get_attrs json in 
    Arg { arg=arg; annotation=annot; type_comment=type_comment; attrs=attrs }

and to_type_ignore json =
  let lineno = json |> member "lineno" |> to_int in 
  let tag = json |> member "tag" |> to_string in 
    TypeIgnore { lineno=lineno; tag=tag }

and to_constant json = 
  try 
    let str = json |> member "value" |> to_string in 
      if String.equal str ellipsis_str then CEllipsis else CString str  
  with Type_error _ -> 
    try 
      CInt (json |> member "value" |> to_int) 
    with Type_error _ -> 
      try 
        CBool (json |> member "value" |> to_bool)
      with Type_error _ -> 
        try 
          CFloat (json |> member "value" |> to_float)
        with _ -> CNone