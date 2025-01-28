module Type = struct
  type expression =
    | EVar of string
    | EInt of int
    | EBool of bool
    | EApp of expression * expression
    | EAbs of string * expression
    | ELet of string * expression * expression
    | EIf of expression * expression * expression

  type t =
    | TVar of string
    | TInt
    | TBool
    | TArrow of t * t
end
