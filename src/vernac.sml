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
          | go (Cmd.Data nm) =
            let val d = {name = nm}
                val tycon = Term.Form d
                val E' = bind E nm Term.Type (SOME tycon)
                val tycon = Term.`` (Option.valOf (Env.findVar E' nm))
                val motiveTy = Term.Pi tycon (Term.ignore Term.Type)
                val P = Var.newvar "P"
                val z = Var.newvar "z"
                val x = Var.newvar "x"
                val elimTy = Term.Pi motiveTy (Term.\\ (P,
                             Term.Pi tycon (Term.\\ (x, Term.Ap (Term.`` P) (Term.`` x)))))
                val elim = Term.Lam motiveTy (Term.\\ (P,
                           Term.Lam tycon (Term.\\ (x,
                           Term.Elim d (Term.\\ (z, Term.Ap (Term.`` P) (Term.`` z)))
                                       (Term.`` x)))))
                val () = print ("Adding lambda-wrapped eliminator: " ^
                                Term.toString elim ^ "\n" ^
                                "Of a priori type " ^ Term.toString elimTy ^ "\n" ^
                                "Of computed type " ^
                                Term.toString (TypeChecker.infertype E' elim) ^ "\n"
                               )
                val E'' = bind E' (nm ^ "_elim") elimTy (SOME elim)
            in
                E''
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
