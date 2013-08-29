(* token.ml: Token type and related functions.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t =
  | Symbol of Symbol.t
  | Keyword of Symbol.t
  | Number of Big_int.big_int
  | String of string
  | Placeholder
  | Placeholder_generic
  | Placeholder_ignore
  | If
  | Then
  | Else
  | True
  | False
  | LeftParen
  | RightParen
  | LeftParenSqr
  | RightParenSqr
  | LeftParenCurl
  | RightParenCurl
  | LetEager
  | LetLazy
  | LetCBN
  | Sep
  | Lambda
  | Force
  | Lazy
  | Leave
  | Newline
  | NewlineSep
  | Eof

let eq x y =
  match x, y with
  | Symbol(s1), Symbol(s2) -> Symbol.eq s1 s2
  | Keyword(s1), Keyword(s2) -> Symbol.eq s1 s2
  | _ -> x = y

let to_string x =
  match x with
  | Symbol(sym) -> "Token.Symbol(" ^ Symbol.to_string sym ^ ")"
  | Keyword(sym) -> "Token.Keyword(" ^ Symbol.to_string sym ^ ")"
  | Number(num) -> "Token.Number(" ^ Big_int.string_of_big_int num ^ ")"
  | String(str) -> "Token.String(\"" ^ str ^ "\")"
  | Placeholder -> "Token.Placeholder"
  | Placeholder_generic -> "Token.Placeholder_generic"
  | Placeholder_ignore -> "Token.Placeholder_ignore"
  | If -> "Token.If"
  | Then -> "Token.Then"
  | Else -> "Token.Else"
  | True -> "Token.True"
  | False -> "Token.False"
  | LeftParen -> "Token.LeftParen"
  | RightParen -> "Token.RightParen"
  | LeftParenSqr -> "Token.LeftParenSqr"
  | RightParenSqr -> "Token.RightParenSqr"
  | LeftParenCurl -> "Token.LeftParenCurl"
  | RightParenCurl -> "Token.RightParenCurl"
  | LetEager -> "Token.LetEager"
  | LetLazy -> "Token.LetLazy"
  | LetCBN -> "Token.LetCBN"
  | Sep -> "Token.Sep"
  | Lambda -> "Token.Lambda"
  | Force -> "Token.Force"
  | Lazy -> "Token.Lazy"
  | Leave -> "Token.Leave"
  | Newline -> "Token.Newline"
  | NewlineSep -> "Token.NewlineSep"
  | Eof -> "Token.Eof"
