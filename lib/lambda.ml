type t =
  | TVar of string (* TVar "something" *)
  | TArrow of t * t (*int -> int*)
  | TInt (* int *)

type term =
  | Var of string (* Var "x" *)
  | Abs of string * t * term (*λf:int.x*)
  | App of term * term (* f x *)
  | Int of int (* useit like Int 42 *)




