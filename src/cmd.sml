structure Cmd =
struct
  datatype 'a t = Def of string * 'a * 'a
               (* | Axiom of string * Term.t
               | Compute of Term.t *)
end
