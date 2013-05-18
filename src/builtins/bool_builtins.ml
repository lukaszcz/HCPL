(* bool_builtins.ml: Basic boolean operation builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node

let bool_not lst =
  match lst with
  | True :: _ -> False
  | False :: _ -> True
  | _ -> failwith "not: type error"

let bool_and lst =
  match lst with
  | True :: True :: _ -> True
  | False :: _ :: _ -> False
  | _ :: False :: _ -> False
  | _ -> failwith "and: type error"

let bool_or lst =
  match lst with
  | False :: False :: _ -> False
  | True :: _ :: _ -> True
  | _ :: True :: _ -> True
  | _ -> failwith "or: type error"

let declare_builtins scope symtab =
  begin
    let scope = Builtin.declare scope (Symtab.find symtab "not") (bool_not, 1, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "and") (bool_and, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "or") (bool_or, 2, true) in
    scope
  end
