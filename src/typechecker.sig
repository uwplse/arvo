signature TYPECHECKER =
sig
  exception TypeError of Term.t * string
  exception Malformed of string

  val infertype : Env.t -> Term.t -> Term.t
  val checktype : Env.t -> Term.t -> Term.t -> bool
end
