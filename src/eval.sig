signature EVAL = sig
    val eval : Env.t -> Term.t -> Term.t

    val equal : Env.t -> Term.t * Term.t -> bool
end
