signature VERNAC =
sig
  val exec : Env.t -> Term.t Cmd.t -> Env.t
  exception Fail of string
  val processStream : Env.t -> (char * Coord.t) Stream.stream -> Env.t

end
