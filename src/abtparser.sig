signature ABTPARSER =
sig
    val term : (string -> Var.t option) -> Ast.t -> Term.t
    val cmd : (string -> Var.t option) -> Ast.t Cmd.t -> Term.t Cmd.t
end
