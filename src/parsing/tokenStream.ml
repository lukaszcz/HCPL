(* tokenStream.mli: Lazy token stream implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type strm_t = Cons of Token.t * Lexing.position * t | Empty
and t = strm_t Lazy.t

exception Eof

let empty = lazy Empty

let cons token pos strm = lazy (Cons(token, pos, strm))

let putback strm lst =
  let rec aux lst acc =
    match lst with
    | (tok, pos) :: t -> aux t (cons tok pos acc)
    | [] -> acc
  in
  aux (List.rev lst) strm

let token strm =
  match Lazy.force strm with
  | Cons(x, _, _) -> x
  | Empty -> raise Eof

let position strm =
  match Lazy.force strm with
  | Cons(_, x, _) -> x
  | Empty -> raise Eof

let next strm =
  match Lazy.force strm with
  | Cons(_, _, tail) -> tail
  | Empty -> raise Eof

let is_empty strm =
  match Lazy.force strm with
  | Cons(_) -> false
  | Empty -> true
