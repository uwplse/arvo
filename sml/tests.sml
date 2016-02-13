structure Tests = struct
open Term
open Var
open Ops

val x = newvar "x"
val ty = $$ (Type, [])
val id = $$ (Lam, [ty, \\ (x, `` x)])
val ap = $$ (Ap, [id, ty])

val A = newvar "A"
val polyid = $$ (Lam, [ty, \\ (A, $$ (Lam, [`` A, \\ (x, `` x)]))])
val tyPolyId = $$ (Pi, [ty, \\ (A, $$ (Pi, [`` A, \\ (x, `` A)]))])

val apidty = $$ (Ap, [polyid, tyPolyId])

val apidid = $$ (Ap, [$$ (Ap, [polyid, tyPolyId]), polyid])


end
