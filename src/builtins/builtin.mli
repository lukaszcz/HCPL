(* builtin.mli: Builtin interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

val make : Symbol.t -> (Env.t -> Node.t) -> int -> Node.call_t -> Node.t
(* (name, func, args_num, call_type) *)

val declare : Scope.t -> Symbol.t -> ((Node.t list -> Node.t) * int * Node.call_t) -> Scope.t * Node.t
