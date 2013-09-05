(* error.mli: Error reporting interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

exception RuntimeError of string

(* runtime_error msg raises RuntimeError(msg) *)
val runtime_error : string -> 'a

val pos_to_string : Lexing.position option -> string

val note : Lexing.position option -> string (* message *) -> unit
val warn : Lexing.position option -> string (* message *) -> unit
val error : Lexing.position option -> string (* message *) -> unit
val fatal : string (* message *) -> unit

val note_count : unit -> int
val warn_count : unit -> int
val error_count : unit -> int
val fatal_count : unit -> int
