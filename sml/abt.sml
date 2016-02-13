functor Abt(O : OPERATOR) :> ABT where type Variable.t = Var.t
                                 where type Operator.t = O.t
=
struct
   open List_Util

   structure Variable = Var
   structure Operator = O

   datatype 'a view =
     ` of Variable.t
   | \ of Variable.t * 'a
   | $ of Operator.t * 'a list

   type info = Var.t
   datatype t =
     FV of Var.t
   | BV of int
   | ABS of info * t
   | OPER of Operator.t * t list

   exception Malformed
   exception NotImplemented

   fun bind x e =
     let fun go n (FV v)        = if Var.equal (v,x) then BV n else FV v
           | go n (BV b)        = BV b
           | go n (ABS (i, e))  = ABS (i, (go (n + 1) e))
           | go n (OPER (f,es)) = OPER (f, map (go n) es)
     in go 0 e
     end

   fun unbind (i:info, e:t) : Var.t * t =
     let val x = Var.newvar (Var.toUserString i)
         fun go n (FV v)        = FV v
           | go n (BV b)        = if n = b then FV x else BV b
           | go n (ABS (i, e))  = ABS (i, go (n + 1) e)
           | go n (OPER (f,es)) = OPER (f, map (go n) es)
     in (x, go 0 e)
     end

   fun into (` v)     = FV v
     | into (\ (v,e)) = ABS (v, bind v e)
     | into ($ fes)   = OPER fes

   fun out (FV v)       = ` v
     | out (BV _)       = raise Malformed
     | out (ABS (i, e)) = \ (unbind (i, e))
     | out (OPER fes)   = $ fes

   fun aequiv (FV x, FV y)                   = Var.equal (x, y)
     | aequiv (BV b1, BV b2)                 = b1 = b2
     | aequiv (ABS (_, e1), ABS (_, e2))     = aequiv (e1, e2)
     | aequiv (OPER (f1,es1), OPER (f2,es2)) = O.equal (f1, f2) andalso zipTest aequiv es1 es2
     | aequiv _                              = false

   fun map f (` v) = ` v
     | map f (\ (v, e)) = \ (v, f e)
     | map f ($ (g, es)) = $ (g, List.map f es)

end
