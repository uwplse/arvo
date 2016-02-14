signature VARIABLE =
sig
  type t

  (* creates a new, globally unique variable *)
  val newvar : string -> t

  (* tests whether two variables are equal *)
  val equal  : (t * t) -> bool

  (* compares two variables.  This is used to allow
     variables as keys into a hash table *)
  val compare : (t * t) -> order

  (* provides a string representation of the globally
     unique variable *)
  val toString : t -> string

  (* provides the string used to create the variable *)
  val toUserString : t -> string
end
