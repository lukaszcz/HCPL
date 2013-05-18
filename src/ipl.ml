(* ipl.ml: IPL main program.

   Copyright (C) 2013 by Łukasz Czajka
*)

let f_interactive = ref false;;
let f_vanilla = ref true;;
let file_count = ref 0;;
let runtime_path = ref "";;

let evaluate node =
  if !f_interactive then
    begin
      print_endline (Node.to_string (Eval.eval node));
      print_string "> "
    end
  else if Error.error_count () + Error.fatal_count () > 0 then
    exit 1
  else
    ignore (Eval.eval node);
  Node.Nil

let run name chan =
  let get_lexbuf name chan =
    let lexbuf = Lexing.from_channel chan
    in
    let pos = lexbuf.Lexing.lex_curr_p
    in
    lexbuf.Lexing.lex_curr_p <- { Lexing.pos_fname = name; Lexing.pos_lnum = pos.Lexing.pos_lnum;
                                  Lexing.pos_bol = pos.Lexing.pos_bol; Lexing.pos_cnum = pos.Lexing.pos_cnum };
    lexbuf
  in
  let lexbuf = get_lexbuf name chan
  in
  file_count := !file_count + 1;
  if !f_vanilla then
    ignore (Parser.parse [lexbuf] evaluate)
  else
    let runtime_lexbuf = get_lexbuf !runtime_path (open_in !runtime_path)
    in
    ignore (Parser.parse [runtime_lexbuf; lexbuf] evaluate);
;;

let argspec = [ ("-i", Arg.Set(f_interactive), "interactive mode");
                (("--interactive", Arg.Set(f_interactive), "interactive mode"));
                (("--vanilla", Arg.Set(f_vanilla), "don't preload the standard runtime"));
                (("-R", Arg.String(fun s -> f_vanilla := false; runtime_path := s), "set runtime path"));
                (("--runtime", Arg.String(fun s -> f_vanilla := false; runtime_path := s), "set runtime path"))
              ]
in
try
  Arg.parse argspec (fun filename -> run filename (open_in filename)) "usage: ipl [file.ipl]";
  if !file_count = 0 then
    begin
      if !f_interactive then
        begin
          print_endline "IPL ver 0.0.1";
          print_endline "Copyright (C) 2013 by Lukasz Czajka";
          print_newline ();
          print_string "> ";
        end;
      run "stdin" stdin;
    end
with
| Sys_error(msg) ->
    Error.fatal msg; exit 1
