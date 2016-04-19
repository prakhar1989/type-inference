type id = string

type op = Add | Mul | Gte | Lte

type primitiveType =
  | TNum
  | TBool
  | T of string
  | TFun of primitiveType * primitiveType
;;

module NameMap = Map.Make(String)

type environment = primitiveType NameMap.t

type expr =
  | NumLit of int
  | BoolLit of bool
  | Val of string
  | Binop of expr * op * expr
  | Fun of id * expr
;;

(* annotated expr -> expr with types *)
type aexpr =
  | ANumLit of int * primitiveType
  | ABoolLit of bool * primitiveType
  | AVal of string * primitiveType
  | ABinop of aexpr * op * aexpr * primitiveType
  | AFun of id * aexpr * primitiveType
;;

(* Unknown type,  resolved type. eg.[(T, TNum); (U, TBool)] *)
type substitutions = (id * primitiveType) list

let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
   returns T(string) of the generated alphabet *)
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; T(Char.escaped (Char.chr c1))
;;

(***************************************************************|
|*******************Annotate Expressions************************|
|***************************************************************|
| Arguments:                                                    |  
|   e -> An expression that has to be annotated                 |
|   env -> An environment map that holds type information of    |
|   user defined variables(in our case values)                  |
|***************************************************************|
| Returns:                                                      |
|   returns an annotated expression of type aexpr that holds    |
|   type information for the given expression.                  |
|***************************************************************| 
| - This method takes every expression/sub-expression in the    |
|   program and assigns some type information to it.            |
| - This type information maybe something concrete like a TNum  |
|   or it could be a unique parameterized type(placeholder) such|
|   as 'a.                                                      |
| - Concrete types are usually assigned when you encounter      |
|   simple literals like 10, true and "hello" and also when the |
|   user has explicity annotated his program with types.        |
| - Whereas, a random type placeholder is assigned when no      |
|   explicit information is available.                          |
| - It may not seem so, but this is a very important function.  |
|   It is a fundamental step in approaching and understanding   |
|   the HMT algorithm that will follow further.                 |
| - HMT algorithm not only infers types of variables and        |
|   functions defined by user but also of every expression and  |
|   sub-expression since most of the inference happens from     |
|   analyzing these expressions only.                           |
| - Hence, this function preps our program for the next steps of|
|   HMT.                                                        |
|***************************************************************)
let rec annotate_expr (e: expr) (env: environment) : aexpr =
  match e with
  | NumLit(n) -> ANumLit(n, TNum)
  | BoolLit(b) -> ABoolLit(b, TBool)
  | Val(x) -> if NameMap.mem x env
    then AVal(x, NameMap.find x env)
    else raise (failwith "variable not defined")
  | Binop(e1, op, e2) ->
    let et1 = annotate_expr e1 env
    and et2 = annotate_expr e2 env
    and new_type = gen_new_type () in
    ABinop(et1, op, et2, new_type)
  | Fun(id, e) ->
    let ae = annotate_expr e env in
    let t = NameMap.find id env in
    AFun(id, ae, TFun(t, gen_new_type ()))

(* returns the type of an annotated expression *)
and type_of (ae: aexpr): primitiveType =
  match ae with
  | ANumLit(_, t) | ABoolLit(_, t) -> t
  | AVal(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AFun(_, _, t) -> t
;;

(* Returns constraints as a list of tuples based on valid operations.  *)
let rec collect_expr (ae: aexpr) : (primitiveType * primitiveType) list =
  match ae with
  | ANumLit(_) | ABoolLit(_) -> []  (* no constraints to impose on literals *)
  | AVal(_) -> []                   (* single occurence of val gives us no info *)
  | ABinop(ae1, op, ae2, t) ->
    let et1 = type_of ae1 and et2 = type_of ae2 in

    (* impose constraints based on binary operator *)
    let opc = match op with
      | Add | Mul -> [(et1, TNum); (et2, TNum); (t, TNum)]
      (* we return et1, et2 since these are generic operators *)
      | Gte | Lte -> [(et1, et2); (t, TBool)]
    in 
    (* opc appended at the rightmost since we apply substitutions right to left *)
    (collect_expr ae1) @ (collect_expr ae2) @ opc
  | AFun(id, ae, t) -> (match t with
      | TFun(idt, ret_type) -> (collect_expr ae) @ [(type_of ae, ret_type)]
      | _ -> raise (failwith "not a function"))
;;

(******************************************************************|
|*************************Substitute*******************************|
|******************************************************************|
|Arguments:                                                        |
|   t -> type in which substitutions have to be made.              |
|   (x, u) -> (type placeholder, resolved substitution)            |
|******************************************************************|
|Returns:                                                          |
|   returns a valid substitution for t if present, else t as it is.|
|******************************************************************|
|- In this method we are given a substitution rule that asks us to |
|  replace all occurences of type placeholder x with u, in t.      |
|- We are required to apply this substitution to t recursively, so |
|  if t is a composite type that contains multiple occurrences of  |
|  x then at every position of x, a u is to be substituted.        |
|- e.g. u -> TNum, x -> 'a, t -> TFun('a, TBool). After            |
|  substitution we will end up with TFun(TNum, TBool).             |
*******************************************************************)
let rec substitute (u: primitiveType) (x: id) (t: primitiveType) : primitiveType =
  match t with
  | TNum | TBool -> t
  | T(c) -> if c = x then u else t
  | TFun(t1, t2) -> TFun(substitute u x t1, substitute u x t2)
;;

(******************************************************************|
|*****************************Apply********************************|
|******************************************************************|
|Arguments:                                                        |
|   subs -> list of substitution rules.                            |
|   t -> type in which substiutions have to be made.               |
|******************************************************************|
|Returns:                                                          |
|   returns t after all the substitutions have been made in it     |
|   given by all the substitution rules in subs.                   |
|******************************************************************|
| - Works from right to left                                       |
| - Effectively what this function does is that it uses            |
|   substitution rules generated from the unification algorithm and|
|   applies it to t. Internally it calls the substitute function   |
|   which does the actual substitution and returns the resultant   |
|   type after substitutions.                                      |
| - Substitution rules: (type placeholder, primitiveType), where we|
|   have to replace each occurence of the type placeholder with the|
|   given primitive type.
|******************************************************************)
let apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;

(* we define two mutually recursive functions that implements the unification algorithm in HMT.
   Unify: takes a list of constraints and returns a list of substitutions *)
let rec unify (constraints: (primitiveType * primitiveType) list) : substitutions =
  match constraints with
  | [] -> []
  | (x, y) :: xs ->
    (* generate substitutions of the rest of the list *)
    let t2 = unify xs in
    (* resolve the LHS and RHS of the constraints from the previous substitutions *)
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2
(* Unify_one: takes LHS and RHS of a constraint and returns a resolved substitution *)
and unify_one (t1: primitiveType) (t2: primitiveType) : substitutions =
  match t1, t2 with
  | TNum, TNum | TBool, TBool -> []
  | T(x), z | z, T(x) -> [(x, z)]
  | _ -> raise (failwith "mismatched types")
;;

(* applies a final set of substitutions on the annotated expr *)
let rec apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  match ae with
  | ABoolLit(b, t) -> ABoolLit(b, apply subs t)
  | ANumLit(n, t) -> ANumLit(n, apply subs t)
  | AVal(s, t) -> AVal(s, apply subs t)
  | ABinop(e1, op, e2, t) -> ABinop(apply_expr subs e1, op, apply_expr subs e2, apply subs t)
  | AFun(id, e, t) -> AFun(id, apply_expr subs e, apply subs t)
;;

(* runs HMTI step-by-step
   1. annotate expression with placeholder types
   2. generate constraints
   3. unify types based on constraints
   4. run the final set of substitutions on still unresolved types
   5. obtain a final annotated expression with resolved types *)
let infer (env: environment) (e: expr) : aexpr =
  let annotated_expr = annotate_expr e env in
  let constraints = collect_expr annotated_expr in
  let subs = unify constraints in
  apply_expr subs annotated_expr
;;

let rec string_of_type (t: primitiveType): string =
  match t with
  | TNum -> "num" | TBool -> "bool"
  | T(x) -> Printf.sprintf "'%s" x
  | TFun(t1, t2) -> let st1 = string_of_type t1
    and st2 = string_of_type t2 in
    Printf.sprintf "(%s -> %s)" st1 st2
;;

let string_of_op (op: op) =
  match op with
  | Add -> "+" | Mul -> "*" | Lte -> "<=" | Gte -> ">=" ;;

let rec string_of_aexpr (ae: aexpr): string =
  match ae with
  | ANumLit(x, t)  -> Printf.sprintf "(%s: %s)" (string_of_int x) (string_of_type t)
  | ABoolLit(b, t) -> Printf.sprintf "(%s: %s)" (string_of_bool b) (string_of_type t)
  | AVal(x, t) -> Printf.sprintf "(%s: %s)" x (string_of_type t)
  | ABinop(e1, op, e2, t) ->
    let s1 = string_of_aexpr e1 in let s2 = string_of_aexpr e2 in
    let sop = string_of_op op in let st = string_of_type t in
    Printf.sprintf "(%s %s %s: %s)" s1 sop s2 st
  | AFun(id, ae, t) ->
    let s1 = string_of_aexpr ae in
    let st = string_of_type t in
    Printf.sprintf "(fun %s -> %s): %s" id s1 st
;;

let rec string_of_expr (e: expr): string =
  match e with
  | NumLit(x) -> string_of_int x
  | BoolLit(b) -> string_of_bool b
  | Val(s) -> s
  | Binop(e1, op, e2) ->
    let s1 = string_of_expr e1 and s2 = string_of_expr e2 in
    let sop = string_of_op op in
    Printf.sprintf "(%s %s %s)" s1 sop s2
  | Fun(id, e) ->
    let s1 = string_of_expr e in Printf.sprintf "(fun %s -> %s)" id s1
;;

(* testing *)
let debug (e: expr) (vals: string list) =
  let env = List.fold_left (fun m x -> NameMap.add x (gen_new_type ()) m) NameMap.empty vals in
  let aexpr = infer env e in
  print_endline (string_of_expr e);
  print_endline (string_of_aexpr aexpr)
;;

let run () =
  let testcases = [
    (Binop(Binop(Val("x"), Add, Val("y")), Mul, Val("z")), ["x"; "y"; "z"]);
    (Binop(Binop(Val("x"), Add, Val("y")), Gte, Val("z")), ["x"; "y"; "z"]);
    (Binop(Binop(Val("x"), Gte, Val("y")), Lte, Val("z")), ["x"; "y"; "z"]);
    (Binop(Binop(Val("x"), Mul, Val("y")), Lte, Binop(Val("z"), Add, Val("w"))), ["x"; "y"; "z"; "w"]);
    (Binop(Binop(Val("x"), Gte, Val("y")), Lte, Binop(Val("z"), Lte, Val("w"))), ["x"; "y"; "z"; "w"]);
    (Fun("x", Binop(Val("x"), Add, NumLit(10))), ["x"]);
    (Fun("x", Binop(NumLit(20), Gte,Binop(Val("x"), Add, NumLit(10)))), ["x"; "y"])]
  in
  List.iter (fun (e, ids) -> debug e ids; print_endline "";) testcases
;;

run ();
