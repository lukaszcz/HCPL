(* tokenStream.mli: Lazy token stream interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

type strm_t
type t = strm_t Lazy.t

exception Eof

val empty : t
val cons : Token.t -> Lexing.position -> t -> t
val putback : t -> (Token.t * Lexing.position) list -> t
val token : t -> Token.t
val position : t -> Lexing.position
val next : t -> t
val is_empty : t -> bool
