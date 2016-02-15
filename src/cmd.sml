structure Cmd =
struct
  datatype t = Def of string * Term.t * Term.t
               (* | Axiom of string * Term.t 
               | Compute of Term.t *)
end
