(* quote.mli: Operations on quoted nodes interface.

Copyright (C) 2013 by Łukasz Czajka

*)

val quote : Node.t -> Env.t -> Node.t
val occurs_check : Node.t -> Node.t -> bool
