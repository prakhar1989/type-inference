%{ open Ast %}

/* Tokens */
%token EOL FUN THINARROW GT LT LPAREN RPAREN
%token PLUS TIMES

%token <int> NUMBER
%token <string> ID

%left LT GT
%left PLUS
%left TIMES

%start main
%type <Ast.expr> main
%%

main:
    expr EOL     { $1 }

expr:
    | ID  { Val($1) }
    | NUMBER { NumLit($1) }
    | expr PLUS expr { Binop($1, Add, $3) }
    | expr TIMES expr { Binop($1, Mul, $3) }
    | expr LT expr { Binop($1, Lt, $3) }
    | expr GT expr { Binop($1, Gt, $3) }
    | LPAREN expr RPAREN { $2 }
    | LPAREN FUN ID THINARROW expr RPAREN { Fun($3, $5) }
