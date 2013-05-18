(* ipl.ml: IPL main program.

   Copyright (C) 2013 by Łukasz Czajka
*)

let run_file filename =
  let lexbuf = Lexing.from_channel (open_in filename)
  and symtab = Symtab.create ()
  in
  let builtins =
    [(fun x -> Arith_builtins.declare_builtins x symtab);
     (fun x -> Generic_builtins.declare_builtins x symtab);
     (fun x -> Bool_builtins.declare_builtins x symtab)]
  in
  let pos = lexbuf.Lexing.lex_curr_p
  in
  lexbuf.Lexing.lex_curr_p <- { Lexing.pos_fname = filename; Lexing.pos_lnum = pos.Lexing.pos_lnum;
                                Lexing.pos_bol = pos.Lexing.pos_bol; Lexing.pos_cnum = pos.Lexing.pos_cnum };
  let strm = Scanner.scan symtab lexbuf
  in
  let node = Parser.parse builtins symtab strm
  in
  ignore (Eval.eval node)
;;

Arg.parse [] run_file "usage"
