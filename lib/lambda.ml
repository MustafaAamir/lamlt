type t =
  | TVar of string (* TVar "something" *)
  | TArrow of t * t (*int -> int*)
  | TInt (* int *)

type term =
  | Var of string (* Var "x" *)
  | Abs of string * t * term (*λf:int.x*)
  | App of term * term (* f x *)
  | Int of int (* useit like Int 42 *)

let rec string_of_type = function
  | TVar s -> s
  | TInt -> "int"
  | TArrow (t1, t2) -> (
      match t1 with
      | _ -> string_of_type t1 ^ " -> " ^ string_of_type t2)



(* does it handle nesting porpely? *)
let rec string_of_term = function
  | Var x -> x
  | Int n -> string_of_int n
  | Abs (x, t, body) ->
      "λ" ^ x ^ ": " ^ string_of_type t ^ ". " ^ string_of_term body
  | App (t1, t2) -> (
      match t2 with
      | App (_, _) -> string_of_term t1 ^ " (" ^ string_of_term t2 ^ ")"
      | _ -> string_of_term t1 ^ " " ^ string_of_term t2)

(* maintain a Hashtbl of vars to avoid redundant increments *)
let gen_var =
  let counter = ref 0 in
  fun base ->
    incr counter;
    base
    ^ string_of_int
        !counter (* x becomes x1 . Should I use x' to sound cooler? *)

let rec free_var = function
  | Int _ -> []
  | Var x -> [ x ]
  | Abs (x, _, t) -> List.filter (( <> ) x) (free_var t)
      (* filter all occurences of var x and recursively free subsequent mem
bers. x -> y -> z *)
  | App (t1, t2) -> free_var t1 @ free_var t2
      (* recursively frees a function application. quadratic merge tho, soo
oooo *)
