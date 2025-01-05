module TypeCheck : sig
  open Types

  val gen_var : string -> string
  val free_var : Type.expression -> string list
  val alpha : string -> Type.expression -> Type.expression -> Type.expression
  val beta_reduce : Type.expression -> Type.expression

  type tycon = string * Type.t
  type scheme = tycon list
end
