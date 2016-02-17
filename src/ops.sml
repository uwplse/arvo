structure Ops =
struct

  datatype t = Type | Pi | Lam | Ap | Form of Data.t | Elim of Data.t

  fun arity Type = []
    | arity Pi   = [0, 1]
    | arity Lam  = [0, 1]
    | arity Ap   = [0, 0]
    | arity (Form _) = []
    | arity (Elim _) = [1, 0]

  fun equal (e1:t, e2:t) = e1 = e2

  fun toString Type     = "Type"
    | toString Pi       = "Pi"
    | toString Lam      = "Lam"
    | toString Ap       = "Ap"
    | toString (Form d) = "Form(" ^ #name d ^ ")"
    | toString (Elim d) = "Elim(" ^ #name d ^ ")"
end
