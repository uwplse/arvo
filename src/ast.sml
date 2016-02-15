structure Ast =
struct
  datatype t = Type | Lam of string * t * t | Pi of string * t * t | Ap of t * t | V of string
end
