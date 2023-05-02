open Frontend

let filename = Sys.argv.(1)
let json = Yojson.Basic.from_file filename
let ast = Json2ast.to_module json 
let _ = print_endline "\n*** Source Program **"
let _ = print_endline (Ast2string.string_of_module ast)
let spy = Py2spy.translate ast 
let inst = Translator.translate spy 

let res = Analyzer.analyze spy inst 
let _ = 
  if res then 
    begin 
      print_endline "Analyzer proved the type-safety of the program"; 
      print_endline "\n*** Target Program **"; 
      print_endline (Spvm.string_of_program inst); 
      Spvm.execute inst
    end
  else print_endline "Analyzer failed to prove the type-safety of the program. The input program will not be executed."

