(* nodeUtils.mli: Node utilities interface. Node graph traversal and transformation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type 'a result_t = Skip of 'a | Continue of 'a

val traverse : (Node.t -> 'a -> 'a result_t) -> Node.t -> 'a -> 'a
val transform :  (Node.t -> Node.t result_t) (* preprocessing *) -> (Node.t -> Node.t) (* postprocessing *) -> Node.t -> Node.t
