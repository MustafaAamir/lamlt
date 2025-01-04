type t =
  | TVar of string
  | TArrow of t * t
  | TInt

type term =
  | Var of string
  | Abs of string * t * term
  | App of term * term
  | Int of int

val string_of_type : t -> string
val string_of_term : term -> string
val gen_var : string -> string
val free_var : term -> string list
val alpha : string -> term -> term -> term
val beta_reduce : term -> term

exception TypeError of string

val type_check : term -> t
