signature TYPECHECKER =
sig
  exception TypeError of Term.t * string

  type context = Term.t Context.dict

  val checktype : context -> Term.t -> Term.t 
end
