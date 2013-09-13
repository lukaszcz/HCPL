(* core_builtins.mli: Core builtins interface. These are builtins
   which are part of the core implementation. Builtins used by
   optional modules should be declared in separate files -- one file
   for each module. E.g. builtins for the module List in the standard
   library should be declared in the files list_builtins.ml(i).

   Copyright (C) 2013 by Łukasz Czajka

*)

val declare_builtins : Scope.t -> Symtab.t -> Scope.t
