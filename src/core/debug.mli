(* debug.mli: Debugging interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

val print : string -> unit
val print_int : int -> unit
val print_bool : bool -> unit
val print_newline : unit -> unit

(* reset timer to zero *)
val reset_timer : unit -> unit
(* return current timer value in ms *)
val timer_value : unit -> int
