functor ABT_Util(A : ABT) : ABT_UTIL =
struct
  open A
  open List_Util

  (* ... your solution goes here ... *)
   exception NotImplemented

   fun freevars e =
     case out e of
        ` v => [v]
      | \ (v, e') => remove Variable.equal v (freevars e')
      | $ (_, es) => collate Variable.equal (List.map freevars es)

   fun isFreeIn x e =
     case out e of
         ` v => Variable.equal (x, v)
       | \ (v, e) => isFreeIn x e
       | $ (f, es) => List.exists (isFreeIn x) es

   fun `` e = into (` e)
   fun \\ e = into (\ e)
   fun $$ e = into ($ e)

   fun subst a x b =
     case out b of
         ` v => if Variable.equal (x, v) then a else `` v
       | \ (v, e') => \\ (v, subst a x e')
       | $ (f, es) => $$ (f, List.map (subst a x) es)

   fun toString e =
     case out e of
         ` v => Variable.toString v
      | \ (v, e) => Variable.toString v ^ ". " ^ toString e
      | $ (f, es) => Operator.toString f ^ "(" ^ String.concatWith ", " (List.map toString es) ^ ")"
end
