module Type = struct
  type expression =
    | EVar of string
    | EInt of int
    | EApp of expression * expression
    | EAbs of string * expression
    | ELet of string * expression * expression

  type t =
    | TVar of string
    | TInt
    | TArrow of t * t
end
