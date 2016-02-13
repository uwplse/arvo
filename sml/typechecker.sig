signature TYPECHECKER =
sig
  exception TypeError of Term.t * string

  type context = Term.t Context.map 

  val checktype : context -> Term.t -> Term.t 
end
