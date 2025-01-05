type t =
  | TVar of string (* TVar "something" *)
  | TArrow of t * t (*int -> int*)
  | TInt
(* int *)
(*| Let of string * expression * expression*)

type term =
  | Var of string (* Var "x" *)
  | Abs of string * t * term (*λf:int.x*)
  | App of term * term (* f x *)
  | Int of int (* useit like Int 42 *)

let rec string_of_type = function
  | TVar s -> s
  | TInt -> "int"
  | TArrow (t1, t2) ->
    let t1' =
      match t1 with
      | TArrow _ -> "(" ^ string_of_type t1 ^ ")"
      | _ -> string_of_type t1
    in
    let t2' =
      match t2 with
      | TArrow _ -> "(" ^ string_of_type t2 ^ ")"
      | _ -> string_of_type t2
    in
    t1' ^ " -> " ^ t2'
;;

(* does it handle nesting porpely? *)
let rec string_of_term = function
  | Var x -> x
  | Int n -> string_of_int n
  | Abs (x, t, body) -> "λ" ^ x ^ ": " ^ string_of_type t ^ ". " ^ string_of_term body
  | App (t1, t2) ->
    (match t2 with
     | App (_, _) -> string_of_term t1 ^ " (" ^ string_of_term t2 ^ ")"
     | _ -> string_of_term t1 ^ " " ^ string_of_term t2)
;;

(* maintain a Hashtbl of vars to avoid redundant increments *)
let gen_var =
  let counter = ref 0 in
  fun base ->
    incr counter;
    base ^ string_of_int !counter (* x becomes x1 . Should I use x' to sound cooler? *)
;;

let rec free_var = function
  | Int _ -> []
  | Var x -> [ x ]
  | Abs (x, _, t) ->
    List.filter (( <> ) x) (free_var t)
    (* filter all occurences of var x and recursively free subsequent mem
bers. x -> y -> z *)
  | App (t1, t2) -> free_var t1 @ free_var t2
;;

(* recursively frees a function application. quadratic merge tho, soo
oooo *)
(*
TODO: Add to README.md
lc is statically scoped.
let stub =
   let discard = "hello" in
   discard is available in this scope
   discard |> print_endline
;;

discard isn't available in this scope.

alpha ~x:variable_to_be_replaced ~s:term_that_replaces_x term
in λy:itn.(x y) our goal is to replace y with Var "x"
   λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
   abstractions create bound variables
   bound variables can't be substituted
   λx.z x introduces the bound variable x.
   Look for free vars
   z is free
   replace
   check for name resolution conflicts
   if alpha "z" "x" λx.z x
   X already exists in this abstraction
   replace occurences of x with x1
   replace z with x
   new expressoin λx1.x x1
   (ignore changing suffixes, free_var list doesn't get flushed)

   if alpha "y" "x" λx.λx.y
   check if y is bound
   if free
   replace all occurrences of x with gen_var(x).
   the new and old expressions will be alpha equiv
   replace y with x
   you'll get smth like λx1.λx1.x
   because λx.λx.x wouldn't be the same

 *)
let rec alpha x s term =
  let () = print_endline ("Alpha: " ^ string_of_term term) in
  (* DEBUG *)
  match term with
  | Var term' when x = term' -> s (* case of direct match return body *)
  | Var term' -> Var term'
  | Int n -> Int n
  | App (t1, t2) -> App (alpha x s t1, alpha x s t2)
  | Abs (y, ty, t) when x = y -> Abs (y, ty, t)
  | Abs (y, ty, t) when not (List.mem y (free_var s)) -> Abs (y, ty, alpha x s t)
  | Abs (y, ty, t) ->
    let gen = gen_var y in
    let t' = alpha y (Var gen) t in
    Abs (gen, ty, alpha x s t')
;;

let beta_reduce term =
  let rec reduce term =
    let () = print_endline ("Beta: " ^ string_of_term term) in
    (* DEBUG *)
    match term with
    | App (Abs (x, _, t1), t2) -> alpha x t2 t1 |> reduce
    | App (t1, t2) ->
      let t1' = reduce t1 in
      let t2' = reduce t2 in
      if t1 = t1' && t2 = t2' then App (t1, t2) else App (t1', t2') |> reduce
    | Abs (x, ty, t) -> Abs (x, ty, reduce t)
    | _ -> term
  in
  try reduce term with
  | Stack_overflow ->
    Printf.printf
      "Stack overflow occurred! possible infinite recursion or the expression is too \
       deeply nested to evaluate.";
    term
;;

(* type checking . will modify*)
exception TypeError of string

(*
   λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
                    Basic rules -- for my own understanding
1. λx:t.y -> x is binded to type t. if t were TInt, then x would be TInt Assuming this is the entire term, this would raise an unbound variable error, since 'y' doesn't exist as an argument to an anon func  (isn't in scope)
  - λx:t.x would be correct
  - λy:t.y would also be correct (alpha conversoin)

2. In an abstraction λx:t.(λy:t'.y) 't' is bound to x. The Abstraction would have a type TArrow(t, TArrow(t', t')) because it is a function that takes a variable of type 't', and returns another abstraction taking the type of t' as an argument
and returns a value of the same type t'

3. In an application of function (λx:t.x) y the abstraction has the type TArrow(t, t), therefore, the variable y would have type t.
*)
let type_check term =
  let rec type_check' env term =
    match term with
    | Int _ -> TInt
    | Var x ->
      (match List.assoc_opt x env with
       | Some t -> t
       | None -> raise (TypeError ("Unbound variable: " ^ x)))
    | Abs (x, t1, body) ->
      let body_type = type_check' ((x, t1) :: env) body in
      TArrow (t1, body_type)
    | App (t1, t2) ->
      (match type_check' env t1 with
       | TArrow (arg_type, ret_type) ->
         let t2_type = type_check' env t2 in
         if t2_type = arg_type
         then ret_type
         else raise (TypeError "Type mismatch in application")
       | _ -> raise (TypeError "Expected function type"))
  in
  type_check' [] term
;;
