module Church : sig
  val true' : Lambda.term
  val false' : Lambda.term
  val zero : Lambda.term
  val interpret : Lambda.term -> int
  val generate : int -> Lambda.term
  val succ' : Lambda.term
  val add' : Lambda.term
  val mul' : Lambda.term
  val pred' : Lambda.term
  val if' : Lambda.term
  val is_zero : Lambda.term
  val and' : Lambda.term
  val or' : Lambda.term
  val not' : Lambda.term
  val xor' : Lambda.term
  val xnor' : Lambda.term
  val lte' : Lambda.term
  val gte' : Lambda.term
  val lt' : Lambda.term
  val gt' : Lambda.term
  val eq' : Lambda.term
  val y' : Lambda.term
  val i' : Lambda.term
  val k' : Lambda.term 
  val s' : Lambda.term 
end
