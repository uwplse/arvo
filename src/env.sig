signature ENV = 
sig
  type binding
  type t

  val newbinding : string -> Term.t -> Term.t option -> binding
  val ty : binding -> Term.t
  val def : binding -> Term.t option

  val empty : t
  val insert : t -> string -> binding -> t
  val find : t -> string -> binding option

  val findVar : t -> string -> Var.t option
end
