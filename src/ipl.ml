(* ipl.ml: IPL main program.

   Copyright (C) 2013 by Łukasz Czajka
*)

Config.init ();;

let f_interactive = ref false;;
let f_vanilla = ref false;;
let file_count = ref 0;;
let runtime_path = ref (Config.stdlib_path () ^ Config.dir_sep () ^ "core.ipl");;

let parse_time = ref 0;;
let gc_time = ref 0;;
let run_time = ref 0;;

let get_lexbuf name chan =
  let lexbuf = Lexing.from_channel chan
  in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = name };
  lexbuf

let run_repl name chan =
  let env = ref Env.empty
  in
  let last_lineno = ref (-1)
  in
  let prompt lineno =
    if lineno > !last_lineno then
      begin
        print_string "> ";
        flush stdout;
      end;
    last_lineno := lineno
  in
  let evaluate node lineno =
    let value = Eval.eval_in node !env
    in
    print_endline (Node.to_string value);
    prompt lineno
  and declare node lineno =
    let value = Eval.eval_in node !env
    in
    env := Env.push !env value;
    print_endline (Node.to_string value);
    prompt lineno
  in
  print_endline "\tIPL version 0.0.1";
  print_endline "\tCopyright (C) 2013 by Lukasz Czajka";
  print_newline ();
  print_string "> ";
  flush stdout;
  let lexbuf = get_lexbuf name chan
  in
  if !f_vanilla then
    ignore (Parser.parse_repl lexbuf None evaluate declare)
  else
    let runtime_lexbuf = get_lexbuf !runtime_path (open_in !runtime_path)
    in
    ignore (Parser.parse_repl lexbuf (Some runtime_lexbuf) evaluate declare)

let run name chan =
  let lexbuf = get_lexbuf name chan
  in
  file_count := !file_count + 1;
  Debug.reset_timer ();
  let (_, node) =
    if !f_vanilla then
      Parser.parse lexbuf None
    else
      let runtime_lexbuf = get_lexbuf !runtime_path (open_in !runtime_path)
      in
      Parser.parse lexbuf (Some runtime_lexbuf)
  in
  parse_time := Debug.timer_value ();
  Debug.reset_timer ();
  Gc.compact ();
  gc_time := Debug.timer_value ();
  if Error.error_count () + Error.fatal_count () = 0 then
    begin
      Debug.reset_timer ();
      ignore (Eval.eval node);
      run_time := Debug.timer_value ();
    end;
  if Config.timing_enabled () then
    begin
      Debug.print_newline ();
      Debug.print ("Parsing time: " ^ string_of_int !parse_time ^ "ms");
      Debug.print ("Parser cleanup time: " ^ string_of_int !gc_time ^ "ms");
      Debug.print ("Program time: " ^ string_of_int !run_time ^ "ms");
    end;
  ()

let usage_msg = "usage: ipl [options] [file.ipl]"

let show_help spec =
  begin
    print_endline usage_msg;
    print_newline ();
    print_endline "Options:";
    let rec loop lst =
      match lst with
      | (x, _, y) :: t -> print_endline ("\t" ^ x ^ " " ^ y); loop t
      | [] -> exit 0
    in
    loop spec
  end

;;

let rec argspec () =
  [ ("-i", Arg.Set(f_interactive), "\t\t\tInteractive (repl) mode");
    ("--interactive", Arg.Set(f_interactive), "\t\tInteractive (repl) mode");
    ("-I", Arg.String(fun s -> Config.prepend_path s), "\t\t\tAdd to include path");
    ("--vanilla", Arg.Set(f_vanilla), "\t\tDon't preload the standard runtime");
    ("-R", Arg.String(fun s -> f_vanilla := false; runtime_path := s), "\t\t\tSet runtime path");
    ("--runtime", Arg.String(fun s -> f_vanilla := false; runtime_path := s), "\t\tSet runtime path");
    ("-t", Arg.Unit(Config.enable_timing), "\t\tDisplay time usage");
    ("--time", Arg.Unit(Config.enable_timing), "\t\tDisplay time usage");
    ("--help", Arg.Unit(fun () -> show_help (argspec ())), "\t\t\tDisplay this list of options")
  ]
in
Config.set_path ["."; Config.stdlib_path ()];
try
  Arg.parse (argspec ()) (fun filename -> run filename (open_in filename)) usage_msg;
  if !file_count = 0 then
    begin
      if !f_interactive then
        run_repl "stdin" stdin
      else
        run "stdin" stdin
    end
with
| Sys_error(msg) ->
    Error.fatal msg; exit 1
