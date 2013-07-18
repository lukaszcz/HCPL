(* core_builtins.mli: Core builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node
open Big_int

let prn lst =
  match lst with
  | String(str) :: _ -> print_endline str; Nil
  | h :: _ -> print_endline (Node.to_string h); Nil
  | _ -> assert false

let myexit lst =
  match lst with
  | Nil :: _ -> exit 0
  | Integer(num) :: _ -> exit (Big_int.int_of_big_int num)
  | _ -> failwith "exit: type error"

(* modules *)

let module_hash = Symbol.Hash.create 16

let load_module lst =
  match lst with
  | x :: init_node :: y :: env ->
      begin
        match x, y with
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

(* random *)

let xrandom lst =
  match lst with
  | (Integer(x)) :: _ -> Integer(big_int_of_int (Random.int (int_of_big_int x)))
  | _ -> failwith "random: type error"

(* strings *)

let concat lst =
  match lst with
  | String(b) :: String(a) :: _ -> String(a ^ b)
  | _ -> failwith "concat: type error"

let to_string lst =
  match lst with
  | x :: _ -> String(Node.to_string x)
  | _ -> failwith "to_string"

(* public interface *)

let declare_builtins scope symtab =
  begin
    Random.self_init ();

    (* set inline builtins names *)

    Node.change_name Node.eq (Symtab.find symtab "=");
    Node.change_name Node.gt (Symtab.find symtab ">");
    Node.change_name Node.lt (Symtab.find symtab "<");
    Node.change_name Node.ge (Symtab.find symtab ">=");
    Node.change_name Node.le (Symtab.find symtab "<=");
    Node.change_name Node.add (Symtab.find symtab "+");
    Node.change_name Node.sub (Symtab.find symtab "-");
    Node.change_name Node.mul (Symtab.find symtab "*");
    Node.change_name Node.idiv (Symtab.find symtab "div");
    Node.change_name Node.xmod (Symtab.find symtab "mod");
    Node.change_name Node.cons (Symtab.find symtab "cons");
    Node.change_name Node.cons_comma (Symtab.find symtab ",");
    Node.change_name Node.cons_lazy (Symtab.find symtab "cons&");
    Node.change_name Node.xfst (Symtab.find symtab "fst");
    Node.change_name Node.xsnd (Symtab.find symtab "snd");
    Node.change_name Node.xhd (Symtab.find symtab "hd");
    Node.change_name Node.xtl (Symtab.find symtab "tl");
    Node.change_name Node.xnot (Symtab.find symtab "not");
    Node.change_name Node.xand (Symtab.find symtab "and");
    Node.change_name Node.xor (Symtab.find symtab "or");

    (* declare inline builtins *)

    let scope = Scope.add_ident scope (Symtab.find symtab "=") Node.eq in
    let scope = Scope.add_ident scope (Symtab.find symtab ">") Node.gt in
    let scope = Scope.add_ident scope (Symtab.find symtab "<") Node.lt in
    let scope = Scope.add_ident scope (Symtab.find symtab ">=") Node.ge in
    let scope = Scope.add_ident scope (Symtab.find symtab "<=") Node.le in
    let scope = Scope.add_ident scope (Symtab.find symtab "+") Node.add in
    let scope = Scope.add_ident scope (Symtab.find symtab "-") Node.sub in
    let scope = Scope.add_ident scope (Symtab.find symtab "*") Node.mul in
    let scope = Scope.add_ident scope (Symtab.find symtab "div") Node.idiv in
    let scope = Scope.add_ident scope (Symtab.find symtab "mod") Node.xmod in
    let scope = Scope.add_ident scope (Symtab.find symtab "cons") Node.cons in
    let scope = Scope.add_ident scope (Symtab.find symtab "cons&") Node.cons_lazy in
    let scope = Scope.add_ident scope (Symtab.find symtab ",") Node.cons_comma in
    let scope = Scope.add_ident scope (Symtab.find symtab "fst") Node.xfst in
    let scope = Scope.add_ident scope (Symtab.find symtab "snd") Node.xsnd in
    let scope = Scope.add_ident scope (Symtab.find symtab "hd") Node.xhd in
    let scope = Scope.add_ident scope (Symtab.find symtab "tl") Node.xtl in
    let scope = Scope.add_ident scope (Symtab.find symtab "not") Node.xnot in
    let scope = Scope.add_ident scope (Symtab.find symtab "and") Node.xand in
    let scope = Scope.add_ident scope (Symtab.find symtab "or") Node.xor in

    (* declare other builtins *)

    let (scope, _) = Builtin.declare scope (Symtab.find symtab "print") (prn, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "exit") (myexit, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_load_module") (load_module, 3, CallByName) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "random") (xrandom, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "^") (concat, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "to_string") (to_string, 1, CallByValue) in

    scope
  end
