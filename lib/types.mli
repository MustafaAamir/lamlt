module Type : sig
  type expression =
    | Var of string
    | Int of int
    | Bool of bool
    | App of expression * expression
    | Abs of string * expression
    | Let of string * expression * expression
    | If of expression * expression * expression

  type t =
    | TVar of string
    | TInt
    | TBool
    | TArrow of t * t
end
