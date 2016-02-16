signature ASTPARSER =
sig
  exception ParseError of string

  val term : (Ast.t, char) ParserCombinators.parser
  val cmd : (Ast.t Cmd.t, char) ParserCombinators.parser
end
