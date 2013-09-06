(* syntax.ml: Syntax specification type.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t =
    Oper of Symbol.t (* operator symbol *) * Opertab.prio_t * int (* assoc *) * int (* arity *) |
    Block of Symbol.t (* block start symbol *) * Symbol.t (* block end symbol *)
