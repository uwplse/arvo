structure Cmd =
struct
  datatype 'a t = Def of string * 'a * 'a
                | Axiom of string * 'a
                | Compute of 'a

  fun map f (Def(nm,x,y)) = Def(nm, f x, f y)
    | map f (Axiom(nm,x)) = Axiom(nm, f x)
    | map f (Compute x) = Compute (f x)
end
