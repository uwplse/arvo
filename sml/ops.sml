structure Ops =
struct

  datatype t = Type | Pi | Lam | Ap

  fun arity Type = []
    | arity Pi   = [0, 1]
    | arity Lam  = [0, 1]
    | arity Ap   = [0, 0]

  fun equal (e1:t, e2:t) = e1 = e2

  fun toString Type = "Type"
    | toString Pi   = "Pi"
    | toString Lam  = "Lam"
    | toString Ap   = "Ap"
end
