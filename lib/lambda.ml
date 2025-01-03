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


