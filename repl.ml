(* REPL *)

let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)
;;

let rec repl () =
  print_string "> ";
  let input = read_line () in
  if input = "" then () else
  try
    let e = parse input in
    print_endline (Ast.string_of_expr e);
    repl ();
  with
  | Failure(msg) -> print_endline msg; repl ()
  | _ -> print_endline "Error"; repl ()
;;


let intro () =
  let template = format_of_string "
Welcome to the REPL.
Type in expressions and let Hindley-Milner Type Inference run its magic.

Out of ideas? Try out a simple lambda expression: (fun x -> x + 10)

" in
  Printf.printf template
;;

intro(); repl ();
