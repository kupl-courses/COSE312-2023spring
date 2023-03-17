%{

%}

%token NEWLINE LPARAN RPAREN PLUS MINUS MULTIPLY
%token <int> NUM

%left PLUS
%left MULTIPLY

%start program
%type <Ast.expr> program

%%

program : exp NEWLINE { $1 }

exp : NUM { Ast.Num ($1) }
| exp PLUS exp { Ast.Add ($1, $3) }
| exp MULTIPLY exp { Ast.Mul ($1, $3) }
