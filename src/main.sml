structure Main =
struct
  fun processTextStream E nm s =
    let fun is_eol s =
          case Stream.front s of
              Stream.Nil => true
            | Stream.Cons (x, s') => x = #"\n"
        val cstream = CoordinatedStream.coordinate is_eol (Coord.init nm)
                                                   (Stream.fromTextInstream s)
    in
        Vernac.processStream E cstream
    end

  fun main (prog_name, args) =
    let
        val () = print "Welcome to Arvo!\n"
        val E = Env.empty
        val E' = processTextStream E "<stdin>" TextIO.stdIn
    in
        0
    end
    handle e => (print ("Exception: " ^ General.exnMessage e ^ "\n"); raise e)
end
