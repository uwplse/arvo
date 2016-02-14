structure PrettyPrinter : PRETTYPRINTER = struct
  open Term
  open Ops

  exception Malformed

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

  fun prettyprint e = 
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
          | getTwo _ = raise Malformed
        fun getAbs e = 
          case out e of
              \ (x, e) => (x, e)
           | _ => raise Malformed
        fun newName m s x = 
          let fun go n str = 
                let val nm = str ^ Int.toString n in
                    if StrSet.member s nm
                    then go (n + 1) str
                    else nm
                end
              val usx = Var.toUserString x
              val nm = if StrSet.member s usx
                       then go 0 usx
                       else usx
          in 
              (VarMap.insert m x nm, StrSet.insert s nm, nm)
          end
        fun go m s p a e =
          case out e of
              ` v => valOf (VarMap.find m v)
            | \ _ => raise Malformed
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

    in
        go VarMap.empty StrSet.empty TOP NONE e
    end

end
