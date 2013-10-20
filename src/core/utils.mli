(* utils.mli: Utilities interface.

Copyright (C) 2013 by Łukasz Czajka

*)

module IntSet : Set.S with type elt = int
module IntMap : Map.S with type key = int

val list_to_string : ('a -> string) -> 'a list -> string
val option_to_string : ('a -> string) -> 'a option -> string
val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a
