(* debug.ml: Debugging implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

let print str =
  prerr_endline str

let print_int i =
  prerr_int i; prerr_newline ()
