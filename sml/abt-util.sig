signature ABT_UTIL =
sig
  include ABT

  val freevars : t -> Variable.t list
  val isFreeIn : Variable.t -> t -> bool
  val subst    : t -> Variable.t -> t -> t

  val `` : Variable.t -> t
  val \\ : Variable.t * t -> t
  val $$ : Operator.t * t list -> t

  val toString : t -> string
end
