(* config.ml: Global configuration module implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

let rpath = ref []

let rtiming = ref false

let init () = ()

let path () = !rpath

let set_path lst = rpath := lst

let prepend_path str = rpath := str :: !rpath

let append_path str = rpath := !rpath @ [str]

let stdlib_path () = "./lib"

let dir_sep () = "/"

let timing_enabled () = !rtiming

let enable_timing () = rtiming := true

let disable_timing () = rtiming := false

(* NOTE: this works only for 32bit or 64bit platforms *)
let int_bits = if max_int lsr 30 = 0 then 30 else 62
