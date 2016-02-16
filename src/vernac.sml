structure Vernac : VERNAC =
struct
  exception CmdError of string

  fun exec E c =
    let val () = print ("Processing command: " ^ PrettyPrinter.cmd c ^ "\n")
        fun go (Cmd.Def (nm, ty, d)) =
          if TypeChecker.checktype E ty Term.Type
          then
              if TypeChecker.checktype E d ty
              then
                  if not (Option.isSome (Env.findVar E nm))
                  then
                      let val b = Env.newbinding nm ty (SOME d)
                      in
                          Env.insert E nm b
                      end
                  else raise CmdError (nm ^ " is already defined!")

              else
                  raise TypeChecker.TypeError (d, nm ^ " expected to have type " ^
                                               PrettyPrinter.term ty ^
                                               ", but instead has type " ^
                                               PrettyPrinter.term (TypeChecker.infertype E d))
          else raise TypeChecker.TypeError (ty, "Expected to have type Type, " ^
                                                "but instead has type " ^
                                            PrettyPrinter.term (TypeChecker.infertype E ty))
    in
        go c
        handle TypeChecker.TypeError (e,msg) => (print ("Type Error in term " ^
                                                        PrettyPrinter.term e ^ "\n" ^
                                                        msg ^ "\n"); E)
             | CmdError msg => (print (msg ^ "\n"); E)
             | TypeChecker.Malformed msg => (print ("Internal error in type checker! " ^
                                                    msg ^ "\n"); E)
    end

  exception Fail of string

  fun processStream E cs =
    let val () = print "processing stream...\n";
        val s = ParserCombinators.transform AstParser.cmd cs
        fun go E s =
          (print ("In environment:\n" ^ Env.toString E) ;
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
