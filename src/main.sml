structure Main =
struct

  fun main (prog_name, args) =
    let fun is_eol s =
          case Stream.front s of
              Stream.Nil => true
            | Stream.Cons (x, s') => x = #"\n"
        val cstream = CoordinatedStream.coordinate is_eol (Coord.init "<stdin>")
                                                   (Stream.fromTextInstream TextIO.stdIn)
        val E = Env.empty
        val () = print "Welcome to Arvo!\n"
        val E' = Vernac.processStream E cstream
    in
        0
    end
    handle e => (print ("Exception: " ^ General.exnMessage e ^ "\n"); raise e)
end
