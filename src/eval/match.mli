(* match.mli: Matching interface.

Copyright (C) 2013 by Łukasz Czajka

*)

exception Unknown

val xmatch : Node.t (* node *) -> Node.t (* pattern *) -> Node.t list (* accumulator *) -> Node.t list
(* equal and equal_quoted raise Unknown if equality cannot be established *)
val equal : Node.t -> Node.t -> bool
val equal_quoted : Node.t -> Node.t -> bool
