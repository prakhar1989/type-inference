(* REPL *)

let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)
;;

module NameMap = Map.Make(String)

let infer (e: Ast.expr): Ast.aexpr =
  let vals = Infer.get_ids e in
  let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty vals in
  Infer.infer env e
;;

let rec repl () =
  print_string "> ";
  let input = read_line () in
  if input = "" then () else
  try
    let e = parse input in
    print_endline (Ast.string_of_aexpr (infer e));
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
