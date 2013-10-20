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
  | Def
  | DynDef
  | LetEager
  | LetLazy
  | LetCBN
  | Sep
  | Lambda
  | Force
  | Lazy
  | Leave
  | ReadExpr
  | ReadProgn
  | Paste
  | TokensStart
  | TokensEnd
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
  | Symbol(sym) -> Symbol.to_string sym
  | Keyword(sym) -> Symbol.to_string sym
  | Number(num) -> Big_int.string_of_big_int num
  | String(str) -> "\"" ^ str ^ "\""
  | Placeholder -> "%"
  | Placeholder_generic -> "%%"
  | Placeholder_ignore -> "%_"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | True -> "true"
  | False -> "false"
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftParenSqr -> "["
  | RightParenSqr -> "]"
  | LeftParenCurl -> "{"
  | RightParenCurl -> "}"
  | Def -> "def"
  | DynDef -> "dyndef"
  | LetEager -> "let"
  | LetLazy -> "let&"
  | LetCBN -> "let&#"
  | Sep -> ";"
  | Lambda -> "\\"
  | Force -> "!"
  | Lazy -> "&"
  | Leave -> "&#"
  | ReadExpr -> "$."
  | ReadProgn -> "$.."
  | Paste -> "#"
  | TokensStart -> "#<"
  | TokensEnd -> ">#"
  | Newline -> "\n"
  | NewlineSep -> "\n;"
  | Eof -> "EOF"
