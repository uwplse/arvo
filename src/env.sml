structure Env : ENV =
struct
  type binding = {var : Var.t, ty : Term.t, def : Term.t option}
  structure StrDict = SplayDict(structure Key = StringOrdered)
  type t = binding StrDict.dict

  fun newbinding nm t d = {var = Var.newvar nm, ty = t, def = d}
  fun ty (b:binding) = let val {ty = t,...} = b in t end
  fun def (b:binding) = let val {def = d,...} = b in d end


  val empty = StrDict.empty
  val insert = StrDict.insert
  val find = StrDict.find

  fun findVar (E:t) nm = Option.map (fn b => let val {var = v,...} = b in v end) (find E nm)
end
