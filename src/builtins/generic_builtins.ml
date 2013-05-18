(* generic_builtins.mli: Generic builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

let eq lst =
  match lst with
  | x :: y :: t -> Node.equal x y
  | _ -> assert false

let prn lst =
  match lst with
  | h :: t -> print_endline (Node.to_string h); Node.Nil
  | _ -> assert false

let declare_builtins scope symtab =
  begin
    let scope = Builtin.declare scope (Symtab.find symtab "=") (eq, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "print") (prn, 1, true) in
    scope
  end
