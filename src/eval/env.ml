(* env.ml: Evaluation environment implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

type t = Node.t list

let empty = []
let nth = List.nth
let push env x = x :: env
