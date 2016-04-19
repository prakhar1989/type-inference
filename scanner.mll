{ open Parser }

rule token = parse
 | [' ' '\r' '\t'] { token lexbuf }
 | ['\n']          { EOL }
 | "fun"           { FUN }
 | "+"             { PLUS }
 | "*"             { TIMES }
 | ['0'-'9']+ as n { NUMBER(int_of_string(n)) }
 | ['a'-'z'] as x  { ID(Char.escaped(x)) }
 | "->"            { THINARROW }
 | ">"             { GT }
 | "<"             { LT }
 | '('             { LPAREN }
 | ')'             { RPAREN }
 | eof             { EOL }
