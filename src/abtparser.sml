structure AbtParser : ABTPARSER =
struct
  open Lexer ParserCombinators
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || ??


  open Ast Term
  structure StrDict = SplayDict(structure Key = StringOrdered)

  fun find_name b f x =
    case StrDict.find b x of
        SOME v => ``v
      | NONE => (case f x of
                     SOME v => ``v
                   | NONE => raise AstParser.ParseError ("Unbound variable " ^ x))

  fun of_ast' b f Ast.Type = $$ (Ops.Type, [])
    | of_ast' b f (Lam (x, A, B)) = let val v = Var.newvar x
                                       val b' = StrDict.insert b x v
                                   in $$ (Ops.Lam, [of_ast' b f A, \\(v, of_ast' b' f B)]) end
    | of_ast' b f (Pi (x, A, B)) = let val v = Var.newvar x
                                       val b' = StrDict.insert b x v
                                  in $$ (Ops.Pi, [of_ast' b f A, \\(v, of_ast' b' f B)]) end
    | of_ast' b f (Ap (A, B)) = $$ (Ops.Ap, [of_ast' b f A, of_ast' b f B])
    | of_ast' b f (V x) = find_name b f x

  fun of_ast f : Ast.t -> Term.t = of_ast' StrDict.empty f

  fun term f = of_ast f

  fun cmd f = Cmd.map (of_ast f)
end
