signature ABTPARSER =
sig
    val term : (string -> Var.t option) -> (Term.t, char) ParserCombinators.parser
    val cmd : (string -> Var.t option) -> (Cmd.t, char) ParserCombinators.parser
end
