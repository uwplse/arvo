structure TypeChecker : TYPECHECKER =
struct
  structure Context = SplayDict(structure Key = VarOrdered)
  type context = Term.t Context.dict

  fun contextToString ctx =
    Context.foldl (fn (v,ty,s) => Var.toUserString v ^ " : " ^
                                    PrettyPrinter.term ty ^ "\n" ^ s) "" ctx

  exception TypeError of context option * Term.t * string
  exception Malformed of string

  open Term
  open Ops

  fun infertype E (e:Term.t) : Term.t =
    let fun getTwo [x, y] : Term.t * Term.t = (x, y)
          | getTwo _ = raise Malformed "getTwo"
        fun getAbs e =
          case out e of
              \ (x, e) => (x, e)
            | _ => raise Malformed "getAbs"
        fun getVar ctx e v =
          case Context.find ctx v of
              SOME ty => ty
            | NONE => (case Env.findBinding E v of
                           SOME b => Env.ty b
                         | NONE => raise TypeError (SOME ctx, e, "Unbound variable " ^ Variable.toString v))
        fun go ctx e =
          case out e of
              ` v => getVar ctx e v
            | \ (x, e) => raise Malformed "unexpected binder in go"
            | $ (f, es) =>
              (case f of
                   Type => Term.Type
                 | Pi   => let val (A, xB) = getTwo es
                               val (x, B) = getAbs xB
                               val tyA = go ctx A
                               val tyB = go (Context.insert ctx x tyA) B
                           in
                               if not (Eval.equal E (tyA, Term.Type))
                               then raise TypeError (SOME ctx, e,
                                                     PrettyPrinter.term A ^
                                                     " has type " ^
                                                     PrettyPrinter.term tyA ^
                                                     " instead of Type")
                               else if not (Eval.equal E (tyB, Term.Type))
                               then raise TypeError (SOME ctx, e,
                                                     PrettyPrinter.term B ^
                                                     " has type " ^
                                                     PrettyPrinter.term tyB ^
                                                     " instead of Type")
                               else Term.Type
                           end
                 | Lam  => let val (A, xB) = getTwo es
                               val (x, B) = getAbs xB
                               val tyA = go ctx A
                           in
                               if not (Eval.equal E (tyA, Term.Type))
                               then raise TypeError (SOME ctx, e,
                                                     "Annotation " ^
                                                     PrettyPrinter.term A ^
                                                     " has type " ^
                                                     PrettyPrinter.term tyA ^
                                                     " instead of Type")
                               else let val tyB = go (Context.insert ctx x A) B
                                    in
                                        $$ (Pi, [A, \\ (x, tyB)])
                                    end
                           end
                 | Ap   => let val (A, B) = getTwo es
                               val tyA = go ctx A
                               val (A1, xA2) =
                                   case out (Eval.eval E tyA) of
                                       $ (Pi, [A1, xA2]) => (A1, xA2)
                                     | _ => raise TypeError (SOME ctx, e,
                                                             "Expected lhs of application " ^
                                                             "to have pi type, but got " ^
                                                             PrettyPrinter.term tyA)
                               val (x, A2) = getAbs xA2
                               val tyB = go ctx B
                           in
                               if not (Eval.equal E (A1, tyB))
                               then raise (TypeError (SOME ctx, e,
                                                      "In application, argument " ^
                                                      PrettyPrinter.term B ^
                                                      " has type " ^
                                                      PrettyPrinter.term tyB ^
                                                      " but expected to have type " ^
                                                      PrettyPrinter.term A1))
                               else Term.subst B x A2
                           end
                 | Form d => Term.Type
                 | Elim d => let val (xP, A) = getTwo es
                                 val (x, P) = getAbs xP
                                 val form = $$ (Form d, [])
                                 val tyP = go (Context.insert ctx x form) P
                                 val tyA = go ctx A
                             in
                                 if not (Eval.equal E (tyP, Term.Type))
                                 then raise TypeError (SOME ctx, P,
                                                       "Motive expected to have type Type" ^
                                                       " but has type " ^
                                                       PrettyPrinter.term tyP)
                                 else if not (Eval.equal E (tyA, form))
                                 then raise TypeError (SOME ctx, A,
                                                       "Discriminee expected to have type " ^
                                                       PrettyPrinter.term form ^
                                                       " but has type " ^
                                                       PrettyPrinter.term tyA)
                                 else Term.subst A x P
                             end
              )
    in
        go Context.empty e
    end

  fun checktype E e ty =
    let val tye = infertype E e
    in Eval.equal E (ty, tye)
    end
end
