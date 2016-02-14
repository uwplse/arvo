signature EVAL = sig
    val eval : Term.t -> Term.t

    val equal : Term.t * Term.t -> bool
end 
