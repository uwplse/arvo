structure Term =
struct
  structure T = ABT_Util(Abt(Ops))
  open T
  val Type = $$ (Ops.Type, [])
end
