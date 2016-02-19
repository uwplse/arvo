structure Term =
struct
  structure T = ABT_Util(Abt(Ops))
  open T
  val Type = $$ (Ops.Type, [])
  fun Pi A xB = $$ (Ops.Pi, [A, xB])
  fun Lam A xB = $$ (Ops.Lam, [A, xB])
  fun Ap A B = $$ (Ops.Ap, [A, B])
  fun Form d = $$ (Ops.Form d, [])
  fun Elim d xP l A = $$ (Ops.Elim d, xP :: l @ [A])
  fun Intro d n = $$ (Ops.Intro (d,n), [])
  fun ignore e = \\ (Var.newvar "_", e)
end
