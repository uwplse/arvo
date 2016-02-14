structure Ast =
struct
  datatype t = Type | Lam of string * t * t | Pi of string * t * t | Ap of t * t | V of string

  open Term
  structure StrDict = SplayDict(structure Key = StringOrdered)

  fun find_name b f x =
    case StrDict.find b x of
        SOME v => ``v
      | NONE => (case StrDict.find (!f) x of
                     SOME v => ``v
                   | NONE => let val v = Var.newvar x in (f := StrDict.insert (!f) x v; ``v) end)

  fun to_abt' b f Type = $$ (Ops.Type, [])
    | to_abt' b f (Lam (x, A, B)) = let val v = Var.newvar x
                                       val b' = StrDict.insert b x v
                                   in $$ (Ops.Lam, [to_abt' b f A, \\(v, to_abt' b' f B)]) end
    | to_abt' b f (Pi (x, A, B)) = let val v = Var.newvar x
                                       val b' = StrDict.insert b x v
                                  in $$ (Ops.Pi, [to_abt' b f A, \\(v, to_abt' b' f B)]) end
    | to_abt' b f (Ap (A, B)) = $$ (Ops.Ap, [to_abt' b f A, to_abt' b f B])
    | to_abt' b f (V x) = find_name b f x

  val to_abt = to_abt' StrDict.empty (ref StrDict.empty)
end
