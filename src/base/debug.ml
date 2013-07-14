(* debug.ml: Debugging implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

let print str =
  prerr_endline str

let print_int i =
  prerr_int i; prerr_newline ()

let print_bool b =
  prerr_endline (if b then "true" else "false")

let print_newline () =
  prerr_newline ()

let rtimer = ref (int_of_float (Sys.time () *. 1000.))

let reset_timer () = rtimer := int_of_float (Sys.time () *. 1000.)

let timer_value () = int_of_float (Sys.time () *. 1000.) - !rtimer
