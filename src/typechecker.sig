signature TYPECHECKER =
sig
  type context
  val contextToString : context -> string
  exception TypeError of context option * Term.t * string
  exception Malformed of string

  val infertype : Env.t -> Term.t -> Term.t
  val checktype : Env.t -> Term.t -> Term.t -> bool
end
