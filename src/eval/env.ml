(* env.ml: Evaluation environment implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

type t = Node.t list

let empty = []
let nth = List.nth
let push env x = x :: env
let rec pop_n env n =
  assert (n >= 0);
  if n > 0 then
    match env with
    | h :: t -> pop_n t (n - 1)
    | [] -> assert false
  else
    env
let length = List.length
