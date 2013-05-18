(* builtin.mli: Builtin interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t = (Node.t list -> Node.t) * int * bool
(* (func, args_num, is_eager (false if lazy)) *)

val declare : Scope.t -> Symbol.t -> t -> Scope.t
