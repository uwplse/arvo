structure Eval : EVAL = struct
  fun eval E e =
    let fun unfold v =
          Option.join (Option.map Env.def (Env.findBinding E v))
        fun go e =
          case Term.out e of
              Term.` v => (case unfold v of
                               SOME e => go e
                             | NONE => Term.`` v)
            | Term.\ (x, e) => Term.\\ (x, go e)
            | Term.$ (f, es) =>
              let val es' = List.map go es in
                  case f of
                      Ops.Ap =>
                      let val [g, v] = es' in
                          case Term.out g of
                              Term.$ (Ops.Lam, [_, body]) =>
                              let val Term.\ (x, e') = Term.out body in
                                  go (Term.subst v x e')
                              end
                            | _ => Term.$$ (f, es')
                      end
                      | Ops.Elim(d) =>
                        let val x = List.last es'
                            val cases = List_Util.butlast (List.tl es')
                        in
                            case Term.out x of
                                Term.$ (Ops.Intro (_,n), _) => List.nth (cases, n)
                             | _ => Term.$$ (f, es')
                        end
                      | _ => Term.$$ (f, es')
              end
    in
        go e
    end

  fun equal E (e1, e2) = Term.aequiv (eval E e1, eval E e2)
end
