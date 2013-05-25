(* opertab.mli: Operator rewriting interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t
type prio_t = After of Symbol.t | Before of Symbol.t | Equal of Symbol.t |
              AfterAppl | BeforeAppl | EqualAppl | First | Last

val assoc_left : int
val assoc_right : int

val empty : t
val add : t -> Symbol.t -> prio_t -> int (* assoc *) -> int (* arity (1 or 2) *) -> t
val drop : t -> Symbol.t -> t
val rewrite : t -> Node.t list -> Node.t
