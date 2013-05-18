(* tokenStream.mli: Lazy token stream interface.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t

exception Eof

val empty : t
val cons : Token.t -> Lexing.position -> t Lazy.t -> t
val token : t -> Token.t
val position : t -> Lexing.position
val next : t -> t
val is_empty : t -> bool
