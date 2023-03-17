let main () = 
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Lexer.token lexbuf in
  let num = Ast.eval ast in
    print_endline (string_of_int num)

let _ =  main ()
