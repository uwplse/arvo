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

structure Parser =
struct
  structure TP = TokenParser(ArvoLangDef)

  open TP
  open ParserCombinators
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || ??

  val force = ParserCombinators.$

  val bound = symbol "_" || identifier

  exception ParseError

  fun pi_names [] ty to = to
    | pi_names (x::xs) ty to = Ast.Pi (x, ty, pi_names xs ty to)

  fun pis [(_,ty)] = ty
    | pis ((ss, ty) :: tys) = pi_names ss ty (pis tys)
    | pis _ = raise ParseError

  fun aps acc [] = acc
    | aps acc (b :: bs) = aps (Ast.Ap (acc, b)) bs

  fun base () = reserved "Type" return Ast.Type ||
                identifier wth (fn x => Ast.V x) ||
                parens (force term)
  and lambda () = symbol "\\" >> bound &&
                  symbol ":" >> force term &&
                  symbol "." >> force term
                  wth Ast.Lam o flat3
  and app () = repeat1 (force base) wth (fn bs => aps (hd bs) (tl bs))
  and expr ()   = parens (repeat bound && symbol ":" >> force term) ||
                  force app wth (fn ty => ([], ty))
  and factor () = separate1 (force expr) (symbol "->") wth pis
  and term () = force lambda || force factor

  val parseTerm = CharParser.parseString (force term)
end
