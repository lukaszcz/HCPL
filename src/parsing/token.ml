(* token.ml: Token type and related functions.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t =
  | Symbol of Symbol.t
  | Keyword of Symbol.t
  | Number of Big_int.big_int
  | If
  | Then
  | Else
  | LeftParen
  | RightParen
  | LeftParenSqr
  | RightParenSqr
  | LeftParenCurl
  | RightParenCurl
  | LetEager
  | LetLazy
  | DoubleSep
  | Sep
  | Lambda
  | Force
  | Lazy
  | Var
  | Eof

let eq x y =
  match x, y with
  | Symbol(s1), Symbol(s2) -> Symbol.eq s1 s2
  | Keyword(s1), Keyword(s2) -> Symbol.eq s1 s2
  | _ -> x = y
