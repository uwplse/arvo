structure Context = SplayMapFn(struct
			         type ord_key = Var.t
				 val compare = Var.compare
			       end)
