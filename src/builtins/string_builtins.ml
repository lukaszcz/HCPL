(* string_builtins.ml: String builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node

let concat lst =
  match lst with
  | String(b) :: String(a) :: _ -> String(a ^ b)
  | _ -> failwith "concat: type error"

let to_string lst =
  match lst with
  | x :: _ -> String(Node.to_string x)
  | _ -> failwith "to_string"

let declare_builtins scope symtab =
  begin
    let scope = Builtin.declare scope (Symtab.find symtab "^") (concat, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "to_string") (to_string, 1, true) in
    scope
  end
