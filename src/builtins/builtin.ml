(* builtin.ml: Builtin implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

let make sym func args_num ct =
  let rec expand node n frm attrs =
    assert (n >= 0);
    if n = 0 then
      node
    else
      let node2 = expand node (n - 1) (frm + 1) None
      in
      Node.Lambda(node2, frm, ct, ref 0, attrs)
  in
  let attrs = Node.Attrs.create (Some(sym)) None
  in
  let node = Node.Builtin(func, args_num, attrs)
  in
  assert (args_num >= 0);
  expand node args_num 0 attrs

let declare scope sym (func, args_num, ct) =
  let b = make sym func args_num ct
  in
  (Scope.add_ident scope sym b, b)
