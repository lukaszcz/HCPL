(* config.mli: Global configuration module interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

val init : unit -> unit

val version : string

val path : unit -> string list
val set_path : string list -> unit
val prepend_path : string -> unit
val append_path : string -> unit

val stdlib_path : unit -> string
val dir_sep : unit -> string

val timing_enabled : unit -> bool
val enable_timing : unit -> unit
val disable_timing : unit -> unit

val is_unsafe_mode : unit -> bool
val set_unsafe_mode : bool -> unit

val is_repl_mode : unit -> bool
val set_repl_mode : bool -> unit

(* the number of bits available for a _positive_ OCaml int *)
val int_bits : int
