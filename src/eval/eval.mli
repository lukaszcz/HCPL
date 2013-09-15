(* eval.mli: Evaluator interface.

Copyright (C) 2013 by Łukasz Czajka

*)

val eval : Node.t -> Node.t (* 'eval x' evaluates x with currently set limit *)
val eval_in : Node.t -> Env.t -> Node.t
val eval_macro : Symtab.t -> Node.t -> Node.t list -> Node.t

(* the following three functions are not reentrant *)
val reduce : Node.t -> Node.t (* reduce x = eval_limited x 1 *)
val eval_limited : Node.t -> int (* limit, -1 for no limit *) -> Node.t
val eval_unlimited : Node.t -> Node.t (* eval_unlimited x = eval_limited x -1 *)

(* extra macro arguments for the current macro invocation *)
val extra_macro_args : unit -> Node.t list
