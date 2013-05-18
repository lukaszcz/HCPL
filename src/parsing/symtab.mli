(* symtab.mli: Symbol table interface. The symbol table maps strings
   to symbols, which may be compared efficiently. The data structure
   is imperative.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t

val create : unit -> t
val find : t -> string -> Symbol.t
