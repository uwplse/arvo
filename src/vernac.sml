structure Vernac : VERNAC = 
struct
  fun exec E (Cmd.Def (nm, ty, d)) = 
    let val b = Env.newbinding nm ty (SOME d) 
    in
        Env.insert E nm b
    end

                                             
end
