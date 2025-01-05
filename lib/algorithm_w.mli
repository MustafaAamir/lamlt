module Type : sig
  type exp =
    | EVar of string
    | EInt of int
    | EApp of exp * exp
    | EAbs of string * exp
    | ELet of string * exp * exp

  type t =
    | TVar of string
    | TInt
    | TArrow of t * t

  val gen_var : string -> string
  val free_var : exp -> string list
  val alpha : string -> exp -> exp -> exp
  val beta_reduce : exp -> exp

  type tycon = string * t
  type scheme = tycon list
end
