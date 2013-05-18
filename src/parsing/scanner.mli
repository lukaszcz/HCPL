(* scanner.mli: Scanner interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

val scan : Symtab.t -> Lexing.lexbuf -> TokenStream.t
val scan_prepend : Symtab.t -> Lexing.lexbuf -> TokenStream.t -> TokenStream.t
