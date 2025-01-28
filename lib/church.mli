module Church : sig
  val true' : Types.Type.expression
  val false' : Types.Type.expression
  val zero : Types.Type.expression
  val interpret : Types.Type.expression -> int
  val generate : int -> Types.Type.expression
  val succ' : Types.Type.expression
  val add' : Types.Type.expression
  val mul' : Types.Type.expression
  val pred' : Types.Type.expression
  val if' : Types.Type.expression
  val is_zero : Types.Type.expression
  val and' : Types.Type.expression
  val or' : Types.Type.expression
  val not' : Types.Type.expression
  val xor' : Types.Type.expression
  val xnor' : Types.Type.expression
  val lte' : Types.Type.expression
  val gte' : Types.Type.expression
  val lt' : Types.Type.expression
  val gt' : Types.Type.expression
  val eq' : Types.Type.expression
  val y' : Types.Type.expression
  val i' : Types.Type.expression
  val k' : Types.Type.expression
  val s' : Types.Type.expression
end
