(* parser.mli: Parser interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

val parse : (Scope.t -> Scope.t) list -> Symtab.t -> TokenStream.t -> Node.t
