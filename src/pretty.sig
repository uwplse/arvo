signature PRETTYPRINTER = sig
    val term : Term.t -> string
    val cmd : Term.t Cmd.t -> string
end
