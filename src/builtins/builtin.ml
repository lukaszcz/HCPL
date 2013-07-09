(* builtin.ml: Builtin implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t = (Node.t list -> Node.t) * int * Node.call_t

let declare scope sym (func, args_num, ct) =
  let rec expand node n frm attrs =
    assert (n >= 0);
    if n = 0 then
      node
    else
      let node2 = expand node (n - 1) (frm + 1) None
      in
      Node.Lambda(node2, frm, ct, ref 0, attrs)
  in
  let node = Node.Builtin(func, args_num, None)
  and attrs = Node.Attrs.create (Some(sym)) None
  in
  assert (args_num >= 0);
  Scope.add_ident scope sym (expand node args_num 0 attrs)
