(* debug.ml: Debugging implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

let print str =
  prerr_endline str

let print_int i =
  prerr_int i; prerr_newline ()

let print_bool b =
  prerr_endline (if b then "true" else "false")
