%{ open Ast %}

/* Tokens */

%token EOL FUN THINARROW GT LT LPAREN RPAREN
%token PLUS TIMES APP
%token <int> NUMBER
%token <string> ID

%nonassoc LPAREN ID NUMBER
%left APP
%left LT GT
%left PLUS
%left TIMES

%start main
%type <Ast.expr> main
%%

main:
    | expr EOL                            { $1 }

expr:
    | ID                                  { Val($1) }
    | NUMBER                              { NumLit($1) }
    | expr PLUS expr                      { Binop($1, Add, $3) }
    | expr TIMES expr                     { Binop($1, Mul, $3) }
    | expr LT expr                        { Binop($1, Lt, $3) }
    | expr GT expr                        { Binop($1, Gt, $3) }
    | LPAREN FUN ID THINARROW expr RPAREN { Fun($3, $5) }
    | expr expr %prec APP                 { App($1, $2) }
    | LPAREN expr RPAREN                  { $2 }
