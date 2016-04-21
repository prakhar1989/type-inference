open Ast

module NameMap = Map.Make(String)

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

let debug (e: expr): string =
  let ids = get_ids e in
  let env = ListLabels.fold_left ~init:NameMap.empty ids
     ~f:(fun m x -> NameMap.add x (Infer.gen_new_type ()) m) in
  let aexpr = Infer.infer env e in
  string_of_type (Infer.type_of aexpr)
;;

let testcases = [|
  NumLit(10);
  BoolLit(true);

  Binop(NumLit(10), Add, NumLit(2));
  Binop(BoolLit(true), Or, BoolLit(false));

  Binop(Binop(Val("x"), Add, Val("y")), Mul, Val("z"));
  Binop(Binop(Val("x"), Add, Val("y")), Gt, Val("z"));
  Binop(Binop(Val("x"), Gt, Val("y")), Lt, Val("z"));
  Binop(Binop(Val("x"), Mul, Val("y")), Lt, Binop(Val("z"), Add, Val("w")));

  Fun("x", Binop(Val("x"), Add, NumLit(10)));
  Fun("x", Binop(NumLit(20), Gt,Binop(Val("x"), Add, NumLit(10))));

  App(Fun("x", Binop(Val("x"), Add, NumLit(10))), NumLit(10));
  Fun("f", Fun("g", Fun("x", App(Val("f"), App(Val("g"), Val("x"))))));
|];;

let literals_check () =
  begin
    Alcotest.(check string) "Integer" "int" (debug testcases.(0));
    Alcotest.(check string) "Boolean" "bool" (debug testcases.(1));
  end
;;

let simple_expr_check () =
  begin
    Alcotest.(check string) "Integer" "int" (debug testcases.(2));
    Alcotest.(check string) "Boolean" "bool" (debug testcases.(3));
  end
;;

let var_expr_check () =
  begin
    Alcotest.(check string) "x + y + z" "int" (debug testcases.(4));
    Alcotest.(check string) "x + y > z" "bool" (debug testcases.(5));
    Alcotest.(check string) "(x > y) < z" "bool" (debug testcases.(6));
    Alcotest.(check string) "(x * y) < (z + w)" "bool" (debug testcases.(7));
  end
;;

let func_decl_check () =
  begin
    Alcotest.(check string) "fun x -> x + 10" "(int -> int)" (debug testcases.(8));
    Alcotest.(check string) "fun x -> (20 > (x + 10))" "(int -> bool)" (debug testcases.(9));
  end
;;


let func_appl_check () =
  begin
    Alcotest.(check string) "(fun x -> x + 10) 10" "int" (debug testcases.(10));
    Alcotest.(check string) "compose function" "(('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))" (debug testcases.(11));
  end
;;

let infer_set = [
  "Literals", `Quick, literals_check;
  "Simple Expr", `Quick, simple_expr_check;
  "Variable Expr", `Quick, var_expr_check;
  "Function Declarations", `Quick, func_decl_check;
  "Function Application", `Quick, func_appl_check;
]

(* Run it *)
let () =
  Alcotest.run "Type-inference testcases" [
    "test_1", infer_set;
  ]
;;
