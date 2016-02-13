structure Eval : EVAL = struct
  fun eval e =
    case Term.out e of
        Term.` v => Term.`` v
     | Term.\ (x, e) => Term.\\ (x, eval e)
     | Term.$ (f, es) =>
       let val es' = List.map eval es in
           case f of
               Ops.Ap =>
               let val [g, v] = es' in
                   case Term.out g of
                       Term.$ (Ops.Lam, [_, body]) =>
                       let val Term.\ (x, e') = Term.out body in
                           Term.subst v x e'
                       end
                    | _ => Term.$$ (f, es')
               end
            | _ => Term.$$ (f, es')
       end

  fun equal (e1, e2) = Term.aequiv (eval e1, eval e2)
end
