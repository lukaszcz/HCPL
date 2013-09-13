(* traversal.mli: Node graph traversal and transformation interface.

   Copyright (C) 2013 by Łukasz Czajka

*)

type 'a result_t = Skip of 'a | Continue of 'a

(* traverse0 and transform0 do not close the node traversed; arguments
   of traverse and transform should contain no free variables *)

val traverse0 : (Node.t -> 'a -> 'a result_t) -> Node.t -> 'a -> 'a
val transform0 :  (Node.t -> Node.t result_t) (* preprocessing *) -> (Node.t -> Node.t) (* postprocessing *) -> Node.t -> Node.t
val traverse : (Node.t -> 'a -> 'a result_t) -> Node.t -> 'a -> 'a
val transform : (Node.t -> int -> Node.t result_t) (* preprocessing *) ->
  (Node.t -> Node.t) (* postprocessing *) -> Node.t -> Node.t
