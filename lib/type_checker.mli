module TypeCheck : sig
  val gen_var : string -> string
  val free_var : Types.Type.expression -> string list

  val alpha
    :  string
    -> Types.Type.expression
    -> Types.Type.expression
    -> Types.Type.expression

  val beta_reduce : Types.Type.expression -> Types.Type.expression
  val code : int ref
  val idx : int ref
  val reset_code : unit -> unit
  val gen_type_var : unit -> Types.Type.t

  type scheme =
    | SAbs of string * scheme * Types.Type.t
    | SApp of scheme * scheme * Types.Type.t
    | SVar of string * Types.Type.t
    | SInt
    | SBool

  val type_of : scheme -> Types.Type.t
  val dummy_types : Types.Type.expression -> scheme

  val collect
    :  scheme list
    -> (Types.Type.t * Types.Type.t) list
    -> (Types.Type.t * Types.Type.t) list

  type subst = (string * Types.Type.t) list

  val occurs : string -> Types.Type.t -> bool
  val substf : Types.Type.t -> string -> Types.Type.t -> Types.Type.t
  val apply : (string * Types.Type.t) list -> Types.Type.t -> Types.Type.t
  val unify_one : Types.Type.t -> Types.Type.t -> (string * Types.Type.t) list
  val unify : (Types.Type.t * Types.Type.t) list -> (string * Types.Type.t) list
  val string_of_type : Types.Type.t -> string
  val string_of_term : Types.Type.expression -> string
  val infer : Types.Type.expression -> Types.Type.t
end
