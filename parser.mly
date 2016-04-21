%{ open Ast %}

/* Tokens */

%token EOL FUN THINARROW GT LT LPAREN RPAREN
%token TRUE FALSE AND OR
%token PLUS TIMES APP
%token <int> NUMBER
%token <string> ID

%nonassoc LPAREN ID NUMBER TRUE FALSE
%left APP
%left AND OR
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
    | TRUE                                { BoolLit(true) }
    | FALSE                               { BoolLit(false) }
    | NUMBER                              { NumLit($1) }
    | expr PLUS expr                      { Binop($1, Add, $3) }
    | expr TIMES expr                     { Binop($1, Mul, $3) }
    | expr LT expr                        { Binop($1, Lt, $3) }
    | expr GT expr                        { Binop($1, Gt, $3) }
    | expr AND expr                       { Binop($1, And, $3) }
    | expr OR expr                        { Binop($1, Or, $3) }
    | LPAREN FUN ID THINARROW expr RPAREN { Fun($3, $5) }
    | expr expr %prec APP                 { App($1, $2) }
    | LPAREN expr RPAREN                  { $2 }
