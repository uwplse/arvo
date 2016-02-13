structure Var :> VARIABLE =
struct
  type t = string * int 

  val counter = ref 0

  fun newvar s = (s, (counter := !counter + 1; !counter))

  fun equal ((_, id1) : t, (_, id2)) = (id1 = id2)

  fun compare ((_, id1), (_, id2)) = Int.compare(id1, id2)

  fun toString (s, id) = s ^ "@" ^ (Int.toString id)
  
  fun toUserString (s, id) = s
end
