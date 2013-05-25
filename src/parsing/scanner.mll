(* scanner.mll: Scanner implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

{

open Big_int
open Lexing

}

let oper = ['-' '+' '=' '~' '`' '@' '#' '$' '%' '^' '*' '|' '/' '?' '.' ':' '<' '>']
let id = ['a'-'z' 'A'-'Z' '_']['-' 'a'-'z' 'A'-'Z' '_' '0'-'9']*['?' '!' '@' '#' '$' '%' '^' '~' '*']? | oper+
let special_oper = [',' '#' '@' '|' ''']

rule read_token symtab = parse
  | ';'                            { Token.Sep }
  | '\\'                           { Token.Lambda }
  | '!'                            { Token.Force }
  | '&'                            { Token.Lazy }
  | '$'                            { Token.Var }
  | special_oper as oper           { Token.Symbol(Symtab.find symtab (Char.escaped oper)) }
  | '-'?['1'-'9']['0'-'9']* as num { Token.Number(big_int_of_string num) }
  | "0x"['0'-'9']+ as num          { Token.Number(big_int_of_int (int_of_string num)) }
  | '0'['0'-'9']+ as num           { Token.Number(big_int_of_int (int_of_string num)) }
  | '0'                            { Token.Number(zero_big_int) }
  | '('                            { Token.LeftParen }
  | ')'                            { Token.RightParen }
  | '['                            { Token.LeftParenSqr }
  | ']'                            { Token.RightParenSqr }
  | '{'                            { Token.LeftParenCurl }
  | '}'                            { Token.RightParenCurl }
  | "if"                           { Token.If }
  | "then"                         { Token.Then }
  | "else"                         { Token.Else }
  | "true"                         { Token.True }
  | "false"                        { Token.False }
  | "let"                          { Token.LetEager }
  | "let!"                         { Token.LetEager }
  | "let&"                         { Token.LetLazy }
  | "/*"                           { comment symtab 0 lexbuf }
  | id as s                        { Token.Symbol(Symtab.find symtab s) }
  | [' ' '\t' '\r']+               { read_token symtab lexbuf }
  | "//"[^'\n']*                   { read_token symtab lexbuf }
  | '\n'                           { new_line lexbuf; Token.Newline }
  | _ as s                         { Token.Symbol(Symtab.find symtab (Char.escaped s)) }
  | eof                            { Token.Eof }

and comment symtab level = parse
  | "*/"        { if level = 0 then read_token symtab lexbuf else comment symtab (level-1) lexbuf }
  | "/*"        { comment symtab (level+1) lexbuf }
  | '\n'        { new_line lexbuf; comment symtab level lexbuf }
  | _           { comment symtab level lexbuf }
  | eof         { Error.error (Some(lexeme_start_p lexbuf)) "Unterminated comment"; Token.Eof }


{

let rec scan_prepend symtab lexbuf cont =
  lazy
    begin
      let token = read_token symtab lexbuf
      in
      if token = Token.Eof then
        Lazy.force (cont ())
      else
        Lazy.force (TokenStream.cons token (lexeme_start_p lexbuf) (scan_prepend symtab lexbuf cont))
    end

let scan symtab lexbuf = scan_prepend symtab lexbuf (fun () -> TokenStream.empty)

}
