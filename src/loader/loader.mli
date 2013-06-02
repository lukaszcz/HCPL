(* loader.mli: Module loader interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

type identtab_t = Node.t Symbol.Map.t

(* raises Not_found if module not found *)
val load_module : string (* module name *) ->
  (Lexing.lexbuf -> identtab_t * Node.t) (* text file parser *) ->
    identtab_t * Node.t
