(* tokenStream.mli: Lazy token stream implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type 'a strm_t = Cons of 'a * 'a strm_t Lazy.t | Empty

type t = (Token.t * Lexing.position) strm_t

exception Eof

let empty = Empty

let cons token pos lstrm = Cons((token, pos), lstrm)

let token = function
  | Cons(x, _) -> fst x
  | Empty -> raise Eof

let position = function
  | Cons(x, _) -> snd x
  | Empty -> raise Eof

let next = function
  | Cons(_, tail) -> Lazy.force tail
  | Empty -> raise Eof

let is_empty = function
  | Cons(_) -> false
  | Empty -> true
