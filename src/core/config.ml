(* config.ml: Global configuration module implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

let version = "git-master"

let data_dir = "."

let bin_dir = "."

let dir_sep () = "/"

let rpath = ref [data_dir ^ dir_sep () ^ "lib"; "."]

let rtiming = ref false

let runsafe = ref false

let rrepl = ref false

let init () = ()

let path () = !rpath

let set_path lst = rpath := lst

let prepend_path str = rpath := str :: !rpath

let append_path str = rpath := !rpath @ [str]

let stdlib_path () = data_dir ^ dir_sep () ^ "lib"

let timing_enabled () = !rtiming

let enable_timing () = rtiming := true

let disable_timing () = rtiming := false

let is_unsafe_mode () = !runsafe

let set_unsafe_mode b = runsafe := b

let is_repl_mode () = !rrepl

let set_repl_mode b = rrepl := b

(* NOTE: this works only for 32bit or 64bit platforms *)
let int_bits = if max_int lsr 30 = 0 then 30 else 62
