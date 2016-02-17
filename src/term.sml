structure Term =
struct
  structure T = ABT_Util(Abt(Ops))
  open T
  val Type = $$ (Ops.Type, [])
  fun Pi A xB = $$ (Ops.Pi, [A, xB])
  fun Lam A xB = $$ (Ops.Lam, [A, xB])
  fun Ap A B = $$ (Ops.Ap, [A, B])
  fun Form nm = $$ (Ops.Form nm, [])
  fun Elim nm xP A = $$ (Ops.Elim nm, [xP, A])
  fun ignore e = \\ (Var.newvar "_", e)
end
