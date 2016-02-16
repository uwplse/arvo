structure AstParser : ASTPARSER  =
struct
  open Lexer ParserCombinators
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || ??

  val force = ParserCombinators.$

  val bound = symbol "_" || identifier

  exception ParseError of string

  fun pi_names [] ty to = to
    | pi_names (x::xs) ty to = Ast.Pi (x, ty, pi_names xs ty to)

  fun pis [(_,ty)] = ty
    | pis ((ss, ty) :: tys) = pi_names ss ty (pis tys)
    | pis _ = raise ParseError "Internal Error!"

  fun aps acc [] = acc
    | aps acc (b :: bs) = aps (Ast.Ap (acc, b)) bs

  fun base () = reserved "Type" return Ast.Type ||
                identifier wth (fn x => Ast.V x) ||
                parens (force term')
  and lambda () = symbol "\\" >> bound &&
                  symbol ":" >> force term' &&
                  symbol "." >> force term'
                  wth Ast.Lam o flat3
  and app () = repeat1 (force base) wth (fn bs => aps (hd bs) (tl bs))
  and expr ()   = parens (repeat bound && symbol ":" >> force term') ||
                  force app wth (fn ty => (["_"], ty))
  and factor () = separate1 (force expr) (symbol "->") wth pis
  and term' () = force lambda || force factor

  val term = force term'

  val defn = (symbol "def" >> identifier &&
                symbol ":" >> term &&
                symbol ":=" >> term)
               wth Cmd.Def o flat3

  val axiom = (symbol "axiom" >> identifier &&
                symbol ":" >> term)
               wth Cmd.Axiom

  val compute = (symbol "compute" >> term) wth Cmd.Compute

  val print : (Ast.t Cmd.t, char) ParserCombinators.parser =
      (symbol "print" >> identifier) wth Cmd.Print

  val cmd = whiteSpace >> (defn || axiom || compute || print) << CharParser.string "."

end
