structure Env : ENV =
struct
  type binding = {ty : Term.t, def : Term.t option}
  structure StrDict = SplayDict(structure Key = StringOrdered)
  structure VarDict = SplayDict(structure Key = VarOrdered)
  type t = {freevars : Var.t StrDict.dict,
            bindings : binding VarDict.dict}


  fun newbinding nm t d = {ty = t, def = d}
  fun ty (b:binding) = let val {ty = t,...} = b in t end
  fun def (b:binding) = let val {def = d,...} = b in d end


  val empty = {freevars = StrDict.empty,
               bindings = VarDict.empty}
  fun insert E nm b =
    let val v = Var.newvar nm
        val {freevars = FV, bindings = B} = E
    in
        { freevars = StrDict.insert FV nm v,
          bindings = VarDict.insert B v b}
    end

  fun findVar (E:t) nm =
    let val {freevars = FV, bindings = B} = E
    in
        StrDict.find FV nm
    end

  fun findBinding E v =
    let val {freevars = FV, bindings = B} = E
    in
        VarDict.find B v
    end

  fun toString E =
    let val {freevars = FV, bindings = B} = E
    in
        VarDict.foldl (fn (v,b,s) => (Var.toUserString v ^ " : " ^
                                     PrettyPrinter.term (ty b) ^ " := " ^
                                     (case def b of
                                         SOME d => PrettyPrinter.term d
                                       | NONE => "<axiom>") ^ "\n") ^ s)
                      ".\n"
                      B
    end

end
