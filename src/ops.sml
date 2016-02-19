structure Ops =
struct

  datatype t = Type | Pi | Lam | Ap | Form of Data.t | Elim of Data.t | Intro of Data.t * int

  fun arity Type = []
    | arity Pi   = [0, 1]
    | arity Lam  = [0, 1]
    | arity Ap   = [0, 0]
    | arity (Form _) = []
    | arity (Elim d) = 1 :: List.map (fn _ => 0) (#constructors d) @ [0]
    | arity (Intro _) = [] (* for now, all datatypes are enumerations,
                              so their constructors take no args *)

  fun equal (e1:t, e2:t) = e1 = e2

  fun toString Type     = "Type"
    | toString Pi       = "Pi"
    | toString Lam      = "Lam"
    | toString Ap       = "Ap"
    | toString (Form d) = "Form(" ^ #name d ^ ")"
    | toString (Elim d) = "Elim(" ^ #name d ^ ")"
    | toString (Intro (d,n)) = "Intro(" ^ #name d ^ "; " ^ Int.toString n ^ ")"
end
