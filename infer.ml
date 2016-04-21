open Ast

(*******************************************************************|
|**********************   Environment   ****************************|
|*******************************************************************|
| - The environment is a map that holds type information of         |
|   variables (in our case values)                                  |
| - A good practice is to have a local environment and global       |
|   environment. This helps in implementing proper scoping rules.   |
|*******************************************************************)
module NameMap = Map.Make(String)
type environment = primitiveType NameMap.t

(* Unknown type,  resolved type. eg.[(T, TNum); (U, TBool)] *)
type substitutions = (id * primitiveType) list

let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
   returns T(string) of the generated alphabet *)
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; T(Char.escaped (Char.chr c1))
;;

(*******************************************************************|
|******************   Annotate Expressions   ***********************|
|*******************************************************************|
| Arguments:                                                        |
|   e -> An expression that has to be annotated                     |
|   env -> An environment map that holds type information of        |
|   user defined variables(in our case values)                      |
|*******************************************************************|
| Returns:                                                          |
|   returns an annotated expression of type aexpr that holds        |
|   type information for the given expression.                      |
|*******************************************************************|
| - This method takes every expression/sub-expression in the        |
|   program and assigns some type information to it.                |
| - This type information maybe something concrete like a TNum      |
|   or it could be a unique parameterized type(placeholder) such    |
|   as 'a.                                                          |
| - Concrete types are usually assigned when you encounter          |
|   simple literals like 10, true and "hello" and also when the     |
|   user has explicity annotated his program with types.            |
| - Whereas, a random type placeholder is assigned when no          |
|   explicit information is available.                              |
| - It may not seem so, but this is a very important function.      |
|   It is a fundamental step in approaching and understanding       |
|   the HMT algorithm that will follow further.                     |
| - HMT algorithm not only infers types of variables and            |
|   functions defined by user but also of every expression and      |
|   sub-expression since most of the inference happens from         |
|   analyzing these expressions only.                               |
| - Hence, this function preps our program for the next steps of    |
|   HMT.                                                            |
|*******************************************************************)
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
  | App(fn, arg) ->
    let afn = annotate_expr fn env in
    let aarg = annotate_expr arg env in
    AApp(afn, aarg, gen_new_type ())

(* returns the type of an annotated expression *)
and type_of (ae: aexpr): primitiveType =
  match ae with
  | ANumLit(_, t) | ABoolLit(_, t) -> t
  | AVal(_, t) -> t
  | ABinop(_, _, _, t) -> t
  | AFun(_, _, t) -> t
  | AApp(_, _, t) -> t
;;

(*********************************************************************|
|***************************   Collect   *****************************|
|*********************************************************************|
|  Arguments:                                                         |
|     ae -> an annotated expression from which a bunch of constraints |
|     have to obtained.                                               |
|*********************************************************************|
|  Returns:                                                           |
|     returns a list of contraints.                                   |
|*********************************************************************|
| - A constraint is a tuple of two primitiveTypes. A strict equality  |
|   is being imposed on the two types.                                |
| - Constraints are generated from the expresssion being analyzed,    |
|   for e.g. for the expression ABinop(x, Add, y, t) we can constrain |
|   the types of x, y, and t to be TNum.                              |
| - To obtain maximum information from expressions and generate       |
|   better constraints operators should not be over-loaded.           |
| - In short, most of the type checking rules will be added here in   |
|   the form of constraints.                                          |
| - Further, if an expression contains sub-expressions, then          |
|   constraints need to be obtained recursively from the              |
|   subexpressions as well.                                           |
| - Lastly, constraints obtained from sub-expressions should be to    |
|   the left of the constraints obtained from the current expression  |
|   since constraints obtained from current expression holds more     |
|   information than constraints from subexpressions and also later   |
|   on we will be working with these constraints from right to left.  |
|*********************************************************************)
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
      | Gt | Lt -> [(et1, et2); (t, TBool)]
      | And | Or -> [(et1, TBool); (et2, TBool); (t, TBool)]
    in
    (* opc appended at the rightmost since we apply substitutions right to left *)
    (collect_expr ae1) @ (collect_expr ae2) @ opc
  | AFun(id, ae, t) -> (match t with
      | TFun(idt, ret_type) -> (collect_expr ae) @ [(type_of ae, ret_type)]
      | _ -> raise (failwith "not a function"))
      
   (* 1. In application expressions, the first expression should be of TFun type or it 
         could be a unknown type placeholder. Otherwise it's an error.
      2. Case 1: TFun(argt, ret_type)
         - In this case the parameter type of the function should be same as that of 
           the argument passed in the function.
         - Second, the return type of the function, will be equal to the return type
           of the function application expression.
      3. Case 2: T(_)  (unknown type placeholder)
         - Since we do not know the type information of the first expression in an 
           application expression, we cannot use the above approach.
         - But we do know that the first expression has to be a function. Also a function
           whose parameter type is same as that of argument type and that has a return type
           same as that of the entire expression.
         - Thus we use this information to impose a contraint on the unknown type placeholder.
   *)
  | AApp(fn, arg, t) -> (match (type_of fn) with
      | TFun(argt, ret_type) -> (collect_expr fn) @ (collect_expr arg) @ [(t, ret_type); (argt, type_of arg)]
      | T(_) -> (collect_expr fn) @ (collect_expr arg) @ [(type_of fn, TFun(type_of arg, t))]
      | _ -> raise (failwith "incorrect function application"))
;;


(******************************************************************|
|**********************   Unification   ***************************|
|**********************    Algorithm    ***************************|
|******************************************************************)


(******************************************************************|
|**********************   Substitute   ****************************|
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
|*************************    Apply    ****************************|
|******************************************************************|
| Arguments:                                                       |
|   subs -> list of substitution rules.                            |
|   t -> type in which substiutions have to be made.               |
|******************************************************************|
| Returns:                                                         |
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
|   given primitive type.                                          |
|******************************************************************)
let apply (subs: substitutions) (t: primitiveType) : primitiveType =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;


(******************************************************************|
|***************************   Unify   ****************************|
|******************************************************************|
| Arguments:                                                       |
|   constraints -> list of constraints (tuple of 2 types)          |
|******************************************************************|
| Returns:                                                         |
|   returns a list of substitutions                                |
|******************************************************************|
| - The unify function takes a bunch of constraints it obtained    |
|   from the collect method and turns them into substitutions.     |
| - It is crucial to remember that these constraints are dependent |
|   on each other, therefore we have separate function unify_one   |
|   and unify.                                                     |
| - Since constraints on the right have more precedence we start   |
|   from the rightmost constraint and unify it by calling the      |
|   unify_one function. unify_one transforms the constraint to a   |
|   substitution. More details given below.                        |
| - Now these substitutions will be applied to both types of the   |
|   second rightmost constraint following which they will be       |
|   unified by again calling the unify_one function.               |
| - This process of unification(unify_one) and substitution(apply) |
|   goes on till all the constraints have been accounted for.      |
| - In the end we get a complete list of substitutions that helps  |
|   resolve types of all expressions in our program.               |
|******************************************************************)
let rec unify (constraints: (primitiveType * primitiveType) list) : substitutions =
  match constraints with
  | [] -> []
  | (x, y) :: xs ->
    (* generate substitutions of the rest of the list *)
    let t2 = unify xs in
    (* resolve the LHS and RHS of the constraints from the previous substitutions *)
    let t1 = unify_one (apply t2 x) (apply t2 y) in
    t1 @ t2

(******************************************************************|
|*************************   Unify One  ***************************|
|******************************************************************|
| Arguments:                                                       |
|   t1, t2 -> two types (one pair) that need to be unified.        |
|******************************************************************|
| Returns:                                                         |
|   returns a substitution rule for the two types if one of them   |
|   is a parameterized type else nothing.                          |
|******************************************************************|
| - A constraint is converted to a substitution here.              |
| - As mentioned several times before a substitution is nothing    |
|   but a resolution rule for a type placeholder.                  |
| - If a constraint yields multiple type resolutions then the      |
|   resolutions should be broken up into a list of constraints and |
|   be passed to the unify function.                               |
| - If both types are concrete then we need not create a new       |
|   substitution rule.                                             |
| - If the types are concrete but dont match then that indicates a |
|   type mismatch. Errors can be as elaborate as required.         |
|******************************************************************)
and unify_one (t1: primitiveType) (t2: primitiveType) : substitutions =
  match t1, t2 with
  | TNum, TNum | TBool, TBool -> []
  | T(x), z | z, T(x) -> [(x, z)]
  
  (* This case is particularly useful when you are calling a function that returns a function *)
  | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)]
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
  | AApp(fn, arg, t) -> AApp(apply_expr subs fn, apply_expr subs arg, apply subs t)
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
  (* reset the type counter after completing inference *)
  type_variable := (Char.code 'a');
  apply_expr subs annotated_expr
;;
