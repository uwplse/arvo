structure TypeChecker : TYPECHECKER =
struct
  exception TypeError of Term.t * string
  exception Malformed

  type context = Term.t Context.dict


  open Term
  open Ops

  fun checktype ctx (e:Term.t) : Term.t =
    let val ty = Term.$$ (Type , [])
        fun getTwo [x, y] : Term.t * Term.t = (x, y)
          | getTwo _ = raise Malformed
        fun getAbs e =
          case out e of
              \ (x, e) => (x, e)
            | _ => raise Malformed
    in
        case out e of
            ` v => (case Context.find ctx v of
                        SOME ty => ty
                      | NONE => raise TypeError (e, "Unbound variable " ^ Variable.toString v))
          | \ (x, e) => raise Malformed
          | $ (f, es) =>
            (case f of
                 Type => ty
               | Pi   => let val (A, xB) = getTwo es
                             val (x, B) = getAbs xB
                             val tyA = checktype ctx A
                             val tyB = checktype (Context.insert ctx x tyA) B
                         in
                             if not (Eval.equal (tyA, ty))
                             then raise TypeError (e, PrettyPrinter.prettyprint A ^
                                                      " has type " ^
                                                      PrettyPrinter.prettyprint tyA ^
                                                      " instead of Type")
                             else if not (Eval.equal (tyB, ty))
                             then raise TypeError (e, PrettyPrinter.prettyprint B ^
                                                      " has type " ^
                                                      PrettyPrinter.prettyprint tyB ^
                                                      " instead of Type")
                             else ty
                         end
               | Lam  => let val (A, xB) = getTwo es
                             val (x, B) = getAbs xB
                             val tyA = checktype ctx A
                         in
                             if not (Eval.equal (tyA, ty))
                             then raise TypeError (e, "Annotation " ^
                                                      PrettyPrinter.prettyprint A ^
                                                      " has type " ^
                                                      PrettyPrinter.prettyprint tyA ^
                                                      " instead of Type")
                             else let val tyB = checktype (Context.insert ctx x A) B
                                  in
                                      $$ (Pi, [A, \\ (x, tyB)])
                                  end
                         end
               | Ap   => let val (A, B) = getTwo es
                             val tyA = checktype ctx A
                             val (A1, xA2) =
                                 case out tyA of
                                     $ (Pi, [A1, xA2]) => (A1, xA2)
                                  | _ => raise Malformed
                             val (x, A2) = getAbs xA2
                             val tyB = checktype ctx B
                         in
                            if not (Eval.equal (A1, tyB))
                            then raise (TypeError (e, "In application, argument " ^
                                                     PrettyPrinter.prettyprint B ^
                                                     " has type " ^
                                                     PrettyPrinter.prettyprint tyB ^
                                                     " but expected to have type " ^
                                                     PrettyPrinter.prettyprint A1))
                            else Term.subst B x A2
                         end)
    end
end
