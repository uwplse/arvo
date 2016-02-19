structure Vernac : VERNAC =
struct
  exception CmdError of string

  fun exec E c =
    let val () = print ("Processing command: " ^ PrettyPrinter.cmd c ^ "\n")
        fun welltyped E e =
          let val ty = TypeChecker.infertype E e
          in ()
          end

        fun expect E e ty =
          if TypeChecker.checktype E e ty
          then ()
          else raise TypeChecker.TypeError
                   (NONE, e,
                    "Expected to have type " ^ PrettyPrinter.term ty ^
                    ", but instead has type " ^
                    PrettyPrinter.term (TypeChecker.infertype E e))

        fun bind E nm ty od =
          let val () = expect E ty Term.Type
              val () = case od of
                           NONE => ()
                        | SOME d => expect E d ty
          in
              if not (Option.isSome (Env.findVar E nm))
              then Env.insert E nm (Env.newbinding nm ty od)
              else raise CmdError (nm ^ " is already defined!")
          end

        fun go (Cmd.Def (nm, ty, d)) =
            let val () = expect E ty Term.Type
                val () = expect E d ty
            in bind E nm ty (SOME d) end
          | go (Cmd.Axiom (nm, ty)) =
            let val () = expect E ty Term.Type
            in bind E nm ty NONE end
          | go (Cmd.Compute e) =
            let val () = welltyped E e
            in print (PrettyPrinter.term e ^ "\n==>\n" ^
                      PrettyPrinter.term (Eval.eval E e) ^ "\n");
               E
            end
          | go (Cmd.Check e) =
            let val ty = TypeChecker.infertype E e
            in
                print (PrettyPrinter.term e ^ " : " ^ PrettyPrinter.term ty ^ "\n");
                E
            end
          | go (Cmd.Print nm) =
            ((case Option.join (Option.map (Env.findBinding E ) (Env.findVar E nm)) of
                 NONE => print ("Unbound variable " ^ nm ^ "\n")
               | SOME b => print (nm ^ " : " ^ PrettyPrinter.term (Env.ty b) ^ " := " ^
                                  (case Env.def b of
                                       NONE => "<axiom>"
                                     | SOME d => PrettyPrinter.term d) ^ "\n"));
             E)
          | go (Cmd.Data (nm,cs)) =
            let val d : Data.t = {name = nm, constructors = cs}
                val tycon = Term.Form d
                val E = bind E nm Term.Type (SOME tycon)
                val tycon = Term.`` (Option.valOf (Env.findVar E nm))
                fun goE E n [] = E
                  | goE E n (c::cs) = goE (bind E c tycon (SOME (Term.Intro d n))) (n + 1) cs
                val E = goE E 0 (#constructors d)
                val motiveTy = Term.Pi tycon (Term.ignore Term.Type)
                val P = Var.newvar "P"
                val z = Var.newvar "z"
                val x = Var.newvar "x"
                fun goElimTy [] ty = ty
                  | goElimTy (c::cs) ty =
                    let val SOME cvar = Env.findVar E c
                    in
                        Term.Pi (Term.Ap (Term.`` P) (Term.`` cvar))
                                (Term.ignore (goElimTy cs ty))
                    end
                val elimTy = Term.Pi motiveTy (Term.\\ (P,
                             goElimTy (#constructors d)
                                      (Term.Pi tycon
                                         (Term.\\ (x, Term.Ap (Term.`` P) (Term.`` x))))))
                val Hvars = List.map (fn c => Var.newvar ("H" ^ c)) (#constructors d)
                fun goElim n [] e = e
                  | goElim n (c::cs) e =
                    let val SOME cvar = Env.findVar E c
                        val Hvar = List.nth (Hvars, n)
                    in
                        Term.Lam (Term.Ap (Term.`` P) (Term.`` cvar))
                                 (Term.\\(Hvar, goElim (n+1) cs e))
                    end

                val elim = Term.Lam motiveTy (Term.\\ (P,
                           goElim 0 (#constructors d)
                           (Term.Lam tycon (Term.\\ (x,
                            Term.Elim d (Term.\\ (z, Term.Ap (Term.`` P) (Term.`` z)))
                                        (List.map (Term.``) Hvars)
                                        (Term.`` x))))))
                val () = print ("Adding lambda-wrapped eliminator: " ^
                                Term.toString elim ^ "\n" ^
                                "Of a priori type " ^ Term.toString elimTy ^ "\n" ^
                                "Of computed type " ^
                                Term.toString (TypeChecker.infertype E elim) ^ "\n"
                               )
                val E = bind E (nm ^ "_elim") elimTy (SOME elim)
            in
                E
            end


    in
        go c
        handle TypeChecker.TypeError (octx, e,msg) =>
               (print ("Type Error" ^
                      (case octx of
                        SOME ctx => " in context\n" ^ TypeChecker.contextToString ctx
                        | NONE => "") ^
                       " in term " ^ PrettyPrinter.term e ^ "\n" ^ msg ^ "\n"); E)
             | CmdError msg => (print (msg ^ "\n"); E)
             | TypeChecker.Malformed msg => (print ("Internal error in type checker! " ^
                                                    msg ^ "\n"); E)
    end

  fun processStream E cs =
    let val () = print "processing stream...\n";
        val s = ParserCombinators.transform AstParser.cmd cs
        fun go E s =
          ((*print ("In environment:\n" ^ Env.toString E) ; *)
           case Stream.front s of
               Stream.Nil => E
             | Stream.Cons (c,s) =>
               let val E' = exec E (AbtParser.cmd (Env.findVar E) c)
                            handle AstParser.ParseError msg =>
                                   (print ("ParseError: " ^ msg ^ "\n"); E)
               in go E' s
               end)
    in
        go E s
    end

end
