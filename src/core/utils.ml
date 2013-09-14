(* utils.ml: Utilities implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

module OrderedIntType =
  struct
    type t = int
    let compare = compare
  end

module IntSet = Set.Make(OrderedIntType)

let list_to_string f lst =
  let rec prn lst =
    match lst with
    | [x] -> f x
    | h :: t -> (f h) ^ "; " ^ prn t
    | [] -> ""
  in
  "[" ^ prn lst ^ "]"

let option_to_string f opt =
  match opt with
  | Some(x) -> "Some(" ^ f x ^ ")"
  | None -> "None"

let try_finally f cleanup =
  try
    let r = f ()
    in
    cleanup ();
    r
  with e ->
    cleanup ();
    raise e
