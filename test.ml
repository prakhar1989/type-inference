open Ast

module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

let rec get_ids (e: expr): id list =
  let rec dedup = function
   | [] -> []
   | x :: y :: xs when x = y -> y :: dedup xs
   | x :: xs -> x :: dedup xs in
  let ids = match e with
   | NumLit(_) | BoolLit(_) -> []
   | Val(x) -> [x]
   | Fun(x, y) -> [x] @ (get_ids y)
   | Binop(e1, _, e2) -> (get_ids e1) @ (get_ids e2)
   | App(fn, arg) -> (get_ids fn) @ (get_ids arg) in
 dedup ids
;;

let debug (e: expr) =
  let ids = get_ids e in
  let env = List.fold_left (fun m x -> NameMap.add x (Infer.gen_new_type ()) m) NameMap.empty ids in
  let aexpr = Infer.infer env e in
  let se = string_of_expr e in
  let expr_type = Infer.type_of aexpr in
  let sae = string_of_type expr_type in
  print_endline (Printf.sprintf "expr: %s\ntype: %s" se sae);
  print_newline ();
;;

let run () =
  let testcases = [
    Binop(Binop(Val("x"), Add, Val("y")), Mul, Val("z"));
    Binop(Binop(Val("x"), Add, Val("y")), Gt, Val("z"));
    Binop(Binop(Val("x"), Gt, Val("y")), Lt, Val("z"));
    Binop(Binop(Val("x"), Mul, Val("y")), Lt, Binop(Val("z"), Add, Val("w")));
    Binop(Binop(Val("x"), Gt, Val("y")), Lt, Binop(Val("z"), Lt, Val("w")));
    Fun("x", Binop(Val("x"), Add, NumLit(10)));
    Fun("x", Binop(NumLit(20), Gt,Binop(Val("x"), Add, NumLit(10))));
    Fun("f", Fun("g", Fun("x", App(Val("f"), App(Val("g"), Val("x"))))));
  ] in
  List.iter debug testcases
;;

run ();
