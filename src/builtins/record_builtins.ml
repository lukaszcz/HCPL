(* record_builtins.ml: Record builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node

let record_get lst =
  match lst with
  | Sym(sym) :: Record(tab) :: _ -> Debug.print "zonk"; Symbol.Map.find sym tab
  | _ -> Debug.print (Utils.list_to_string Node.to_string lst); assert false

let declare_builtins scope symtab =
  begin
    let scope = Builtin.declare scope (Symtab.find symtab "__ipl_record_get") (record_get, 2, true) in
    scope
  end
