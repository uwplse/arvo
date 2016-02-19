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
        fun expect ctx e tye ty_expect =
          if not (Eval.equal E (tye, ty_expect))
          then
              raise TypeError (SOME ctx, e,
                               "has type " ^ PrettyPrinter.term tye ^
                               " but expected to have type " ^ PrettyPrinter.term ty_expect)
          else ()
        fun getPi ctx e ty =
              case out (Eval.eval E ty) of
                  $ (Pi, [A1, xA2]) => (A1, xA2)
                | _ => raise TypeError (SOME ctx, e,
                                        "Expected lhs of application " ^
                                        "to have pi type, but got " ^
                                        PrettyPrinter.term ty)

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
                               val () = expect ctx A tyA Term.Type
                               val tyB = go (Context.insert ctx x A) B
                               val () = expect ctx B tyB Term.Type
                           in
                               Term.Type
                           end
                 | Lam  => let val (A, xB) = getTwo es
                               val (x, B) = getAbs xB
                               val tyA = go ctx A
                               val () = expect ctx A tyA Term.Type
                               val tyB = go (Context.insert ctx x A) B
                           in
                               Term.Pi A (\\ (x, tyB))
                           end
                 | Ap   => let val (A, B) = getTwo es
                               val tyA = go ctx A
                               val (A1, xA2) = getPi ctx A tyA
                               val (x, A2) = getAbs xA2
                               val tyB = go ctx B
                               val () = expect ctx B tyB A1
                           in
                               subst B x A2
                           end
                 | Form d => Term.Type
                 | Elim d => let val xP = List.hd es
                                 val (x, P) = getAbs xP
                                 val form = Term.Form d
                                 val tyP = go (Context.insert ctx x form) P
                                 val () = expect ctx P tyP Term.Type

                                 val cases = List_Util.butlast (List.tl es)
                                 fun checkcases [] _ = ()
                                   | checkcases (H::Hs) (cnm::cnms) =
                                     let val ty = go ctx H
                                         val SOME cvar = Env.findVar E cnm
                                         val () = expect ctx H ty (subst (`` cvar) x P)
                                     in
                                         checkcases Hs cnms
                                     end
                                   | checkcases _ _ = raise Malformed "Elim"
                                 val () = checkcases cases (#constructors d)

                                 val A = List.last es
                                 val tyA = go ctx A
                                 val () = expect ctx A tyA form

                             in
                                 subst A x P
                             end
                 | Intro (d,n) => Term.Form d
              )
    in
        go Context.empty e
    end

  fun checktype E e ty =
    let val tye = infertype E e
    in Eval.equal E (ty, tye)
    end
end
