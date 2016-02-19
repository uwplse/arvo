structure PrettyPrinter : PRETTYPRINTER = struct
  open Term
  open Ops

  exception PrettyMalformed of string

  datatype prec = BOT | AP | LAM | TOP

  fun prec_lt TOP _ = false
    | prec_lt _ TOP = true
    | prec_lt LAM _ = false
    | prec_lt _ LAM = true
    | prec_lt AP _  = false
    | prec_lt _ AP  = true
    | prec_lt BOT BOT = false

  datatype assoc = LEFT | RIGHT | NONE

  structure VarMap = SplayDict(structure Key = VarOrdered)
  structure StrSet = SplaySet(structure Elem = StringOrdered)

  fun term e =
    let fun prec_of_op Lam = LAM
          | prec_of_op Pi = LAM
          | prec_of_op Ap = AP
          | prec_of_op _ = BOT
        fun assoc_of_prec LAM = RIGHT
          | assoc_of_prec AP = LEFT
          | assoc_of_prec TOP = NONE
          | assoc_of_prec BOT = NONE
        fun no_parens p1 p2 a =
          prec_lt p1 p2 orelse (p1 = p2 andalso a = assoc_of_prec p1)
        fun getTwo [x, y] = (x,y)
          | getTwo _ = raise PrettyMalformed "getTwo"
        fun getAbs e =
          case out e of
              \ (x, e) => (x, e)
           | _ => raise PrettyMalformed "getAbs"
        val freeM = ref VarMap.empty
        val freeS = ref StrSet.empty
        fun newName m s x =
          let fun go n str =
                let val nm = str ^ Int.toString n in
                    if StrSet.member s nm orelse StrSet.member (!freeS) nm
                    then go (n + 1) str
                    else nm
                end
              val usx = Var.toUserString x
              val nm = if StrSet.member s usx orelse StrSet.member (!freeS) usx
                       then go 0 usx
                       else usx
          in
              (VarMap.insert m x nm, StrSet.insert s nm, nm)
          end
        fun doVar m s v =
          (case VarMap.find m v of
               SOME s => s
             | Option.NONE =>
               (case VarMap.find (!freeM) v of
                    SOME s => s
                  | Option.NONE =>
                    let val (fM', fS', nm:string) = newName (!freeM) (!freeS) v
                    in
                        freeM := fM';
                        freeS := fS';
                        nm
                    end))
        fun go m s p a e =
          case out e of
              ` v => doVar m s v
            | \ _ => raise PrettyMalformed "unexpected binder in go"
            | $ (f, es) =>
              if not (no_parens (prec_of_op f) p a)
              then "(" ^ go m s TOP NONE e ^ ")"
              else case f of
                      Type => "Type"
                    | Lam => let val (A, xB) = getTwo es
                                 val (x, B) = getAbs xB
                                 val (m', s', nm) = newName m s x
                             in
                                 "\\" ^ (if isFreeIn x B then nm else "_") ^ " : " ^
                                 go m s TOP NONE A ^ ". " ^ go m' s' LAM RIGHT B
                             end
                    | Pi => let val (A, xB) = getTwo es
                                val (x, B) = getAbs xB
                                val (m', s', nm) = newName m s x
                            in
                                 (if isFreeIn x B
                                  then "(" ^ nm ^ " : " ^ go m s TOP NONE A ^ ")"
                                  else go m s LAM LEFT A) ^
                                 " -> " ^ go m' s' LAM RIGHT B
                             end
                    | Ap => let val (A, B) = getTwo es
                            in
                                go m s AP LEFT A ^ " " ^ go m s AP RIGHT B
                            end
                    | (Form d) => #name d ^ "()"
                    | (Elim d) => let val xP = List.hd es
                                      val cases = List_Util.butlast (List.tl es)
                                      val A = List.last es
                                      val (x, P) = getAbs xP
                                      val (m', s', nm) = newName m s x
                                      fun goCases [] = ", "
                                        | goCases (c::cs) =
                                          ", " ^ go m s TOP NONE c ^ goCases cs
                                  in
                                      #name d ^ "_elim(" ^
                                      (if isFreeIn x P then nm else "_") ^ ". " ^
                                      go m' s' TOP NONE P ^
                                      goCases cases ^
                                      go m s TOP NONE A ^ ")"
                                  end
                    | (Intro (d,n)) => List.nth (#constructors d, n) ^ "()"

    in
        go VarMap.empty StrSet.empty TOP NONE e
    end

  fun cmd c =
    let val s =
            case c of
                Cmd.Def(nm,ty,d) => "def " ^ nm ^ " : " ^ term ty ^ " := " ^ term d
              | Cmd.Axiom(nm,ty) => "axiom " ^ nm ^ " : " ^ term ty
              | Cmd.Compute(e) => "compute " ^ term e
              | Cmd.Check(e) => "check " ^ term e
              | Cmd.Print(nm) => "print " ^ nm
              | Cmd.Data(nm,cs) => "data " ^ nm ^ " := " ^
                                   String.concatWith " | " cs
    in
        s ^ "."
    end

end
