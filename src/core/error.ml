(* error.ml: Error reporting implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Lexing

exception RuntimeError of Node.t

(* 'rec' to prevent inlining *)
let rec runtime_error msg = raise (RuntimeError(Node.String(msg)))

let err_count = Array.make 4 0

let max_error_count = 20

let pos_to_string apos =
  match apos with
  | Some(pos) -> pos.pos_fname ^ ":" ^ string_of_int(pos.pos_lnum) ^ ":" ^
      string_of_int(pos.pos_cnum - pos.pos_bol) ^ ":"
  | None -> ""

let print_error idx pos msg =
  prerr_endline (pos_to_string pos ^ (if pos = None then "" else " ") ^ msg);
  err_count.(idx) <- err_count.(idx) + 1;
  if err_count.(idx) > max_error_count then
    begin
      prerr_endline "Too many errors, exiting...";
      exit 1;
    end;
  ()

let note pos msg = print_error 0 pos ("note: " ^ msg)
let warn pos msg = print_error 1 pos ("warning: " ^ msg)
let error pos msg = print_error 2 pos ("error: " ^ msg)
let fatal msg = print_error 3 None ("fatal error: " ^ msg)

let note_count () = err_count.(0)
let warn_count () = err_count.(1)
let error_count () = err_count.(2)
let fatal_count () = err_count.(3)
