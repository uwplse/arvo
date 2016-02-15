structure ArvoLangDef :> LANGUAGE_DEF =
struct
  open ParserCombinators CharParser
  infix 1 ||

  type scanner = char charParser

  val commentStart = SOME "@"
  val commentEnd = SOME "@"

  val commentLine = NONE
  val nestedComments = false

  val identStart = letter
  val identLetter = letter || digit || oneOf (String.explode "'_")

  val opStart = fail "operators not supported" : scanner
  val opLetter = opStart

  val reservedNames = ["Type"]
  val reservedOpNames = []

  val caseSensitive = true

end
