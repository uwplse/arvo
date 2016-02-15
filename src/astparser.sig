signature ASTPARSER =
sig
  exception ParseError of string

  val term : (Ast.t, char) ParserCombinators.parser
end
