(* eval.ml: Evaluator interface.

Copyright (C) 2013 by Łukasz Czajka

*)

(* val apply : Node.t -> Node.t -> Node.t *)
val reduce : Node.t -> Node.t (* reduce x = eval_limited x 1 *)
val eval : Node.t -> Node.t (* eval x = eval_limited x -1 *)
val eval_limited : Node.t -> int (* limit, -1 for no limit *) -> Node.t
