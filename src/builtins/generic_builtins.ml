(* generic_builtins.mli: Generic builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node

let eq lst =
  match lst with
  | x :: y :: _ -> Node.equal x y
  | _ -> assert false

let prn lst =
  match lst with
  | String(str) :: _ -> print_endline str; Nil
  | h :: _ -> print_endline (Node.to_string h); Nil
  | _ -> assert false

let myexit lst =
  match lst with
  | Nil :: _ -> exit 0
  | Integer(num) :: _ -> exit (Big_int.int_of_big_int num)
  | _ -> assert false

let module_hash = Symbol.Hash.create 16

let load_module lst =
  match lst with
  | Delayed(x) :: init_node :: Delayed(y) :: env ->
      begin
        match !x, !y with
        | Integer(bfrm), Sym(sym) ->
            begin
              try
                Symbol.Hash.find module_hash sym
              with Not_found ->
                let frm = Big_int.int_of_big_int bfrm
                in
                assert (frm <= Env.length env);
                let env2 = Env.pop_n env (Env.length env - frm - 1)
                in
                let node = Eval.eval_in init_node env2
                in
                Symbol.Hash.add module_hash sym node;
                node
            end
        | _ -> assert false
      end
  | _ -> Debug.print (Utils.list_to_string Node.to_string lst); assert false

let cons lst =
  match lst with
  | y :: x :: _ -> Node.Cons(x, y)
  | _ -> assert false

let lst_fst lst =
  match lst with
  | Node.Cons(x, y) :: _ -> x
  | _ -> failwith "not a cons"

let lst_snd lst =
  match lst with
  | Node.Cons(x, y) :: _ -> y
  | _ -> failwith "not a cons"

let declare_builtins scope symtab =
  begin
    let scope = Builtin.declare scope (Symtab.find symtab "=") (eq, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "print") (prn, 1, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "exit") (myexit, 1, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "__ipl_load_module") (load_module, 3, false) in
    let scope = Builtin.declare scope (Symtab.find symtab "cons") (cons, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "cons&") (cons, 2, false) in
    let scope = Builtin.declare scope (Symtab.find symtab "fst") (lst_fst, 1, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "snd") (lst_snd, 1, true) in
    scope
  end
