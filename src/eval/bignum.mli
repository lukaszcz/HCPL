(* bignum.mli: Big numbers encoded in nodes -- interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

val from_int : int -> Node.t
val from_big_int : Big_int.big_int -> Node.t
val to_int : Node.t -> int
val to_big_int : Node.t -> Big_int.big_int
val is_number : Node.t -> bool
val gt : Node.t -> Node.t -> Node.t
val ge : Node.t -> Node.t -> Node.t
val add : Node.t -> Node.t -> Node.t
val sub : Node.t -> Node.t -> Node.t
val mul : Node.t -> Node.t -> Node.t
val idiv : Node.t -> Node.t -> Node.t
val modulo : Node.t -> Node.t -> Node.t
