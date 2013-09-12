(* match.mli: Matching interface.

Copyright (C) 2013 by Łukasz Czajka

*)

exception Unknown

(* xmatch raises Exit if node does not match pattern, and returns the
   list of values matched to placeholders if the match is successful *)
val xmatch : Node.t (* node *) -> Node.t (* pattern *) -> Node.t list (* accumulator *) -> Node.t list

(* equal and equal_quoted raise Unknown if equality cannot be established *)
val equal : Node.t -> Node.t -> bool
val equal_quoted : Node.t -> Node.t -> bool
