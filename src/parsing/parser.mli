(* parser.mli: Parser interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

val parse : Lexing.lexbuf list -> (Node.t -> Node.t) (* handler *) -> Node.t
(* The handler is invoked for every finished compilation unit. The
   result of the handler is taken as the node corresponding to the
   compilation unit. *)
