(* core_builtins.mli: Core builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node
open Big_int

let eq_builtin = ref Nil
let eq lst =
  match lst with
  | y :: x :: _ ->
      let ret = Node.equal x y
      in
      if ret = Nil then
        Node.Leave(Node.mkappl [!eq_builtin; x; y] None)
      else
        ret
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

(* records *)

let record_get lst =
  match lst with
  | Sym(sym) :: Record(tab) :: _ -> Symbol.Map.find sym tab
  | _ -> Debug.print (Utils.list_to_string Node.to_string lst); assert false

(* conses *)

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

(* arithmetic *)

let add lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> Integer(add_big_int a b)
  | _ -> failwith "(+): type error"

let sub lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> Integer(sub_big_int a b)
  | _ -> failwith "(-): type error"

let mul lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> Integer(mult_big_int a b)
  | _ -> failwith "(*): type error"

let div lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> Integer(div_big_int a b)
  | _ -> failwith "div: type error"

let xmod lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> Integer(mod_big_int a b)
  | _ -> failwith "div: type error"

let gt lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> if gt_big_int a b then True else False
  | _ -> failwith "(>): type error"

let lt lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> if lt_big_int a b then True else False
  | _ -> failwith "(<): type error"

let ge lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> if ge_big_int a b then True else False
  | _ -> failwith "(>=): type error"

let le lst =
  match lst with
  | (Integer(b)) :: (Integer(a)) :: _ -> if le_big_int a b then True else False
  | _ -> failwith "(<=): type error"

(* booleans *)

let not_builtin = ref Nil
let bool_not lst =
  match lst with
  | True :: _ -> False
  | False :: _ -> True
  | h :: _ -> Node.Leave(mkappl [!not_builtin; h] None)
  | _ -> assert false

let and_builtin = ref Nil
let bool_and lst =
  match lst with
  | True :: True :: _ -> True
  | False :: _ :: _ -> False
  | _ :: False :: _ -> False
  | y :: x :: _ -> Node.Leave(mkappl [!and_builtin; x; y] None)
  | _ -> assert false

let or_builtin = ref Nil
let bool_or lst =
  match lst with
  | False :: False :: _ -> False
  | True :: _ :: _ -> True
  | _ :: True :: _ -> True
  | y :: x :: _ -> Node.Leave(mkappl [!or_builtin; x; y] None)
  | _ -> assert false

(* strings *)

let concat lst =
  match lst with
  | String(b) :: String(a) :: _ -> String(a ^ b)
  | _ -> failwith "concat: type error"

let to_string lst =
  match lst with
  | x :: _ -> String(Node.to_string x)
  | _ -> failwith "to_string"

(* matching *)

let match1 lst =
  match lst with
  | z2 :: z1 :: y :: x :: t ->
      let eval x =
        match x with
        | Integer(_) | String(_) | Record(_) | Sym(_) | True | False |
          Placeholder | Ignore | Cons(_) | Nil | Quoted(_) -> x
        | _ -> (Eval.eval_in x t)
      in
      let node = eval x
      and pat = eval y
      in
      let (m, args) = Node.matches node pat
      in
      if m then
        if args = [] then
          z1
        else
          Node.mkappl (z1 :: args) None
      else
        z2
  | _ -> assert false

(* public interface *)

let declare_builtins scope symtab =
  begin
    let (scope, builtin) = Builtin.declare scope (Symtab.find symtab "=") (eq, 2, CallByValue) in
    eq_builtin := builtin;
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "print") (prn, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "exit") (myexit, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_load_module") (load_module, 3, CallByName) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_record_get") (record_get, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "cons") (cons, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "cons&") (cons, 2, CallByNeed) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "fst") (lst_fst, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "snd") (lst_snd, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "+") (add, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "-") (sub, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "*") (mul, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "div") (div, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "mod") (xmod, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab ">") (gt, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "<") (lt, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab ">=") (ge, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "<=") (le, 2, CallByValue) in
    let (scope, builtin) = Builtin.declare scope (Symtab.find symtab "not") (bool_not, 1, CallByValue) in
    not_builtin := builtin;
    let (scope, builtin) = Builtin.declare scope (Symtab.find symtab "and") (bool_and, 2, CallByValue) in
    and_builtin := builtin;
    let (scope, builtin) = Builtin.declare scope (Symtab.find symtab "or") (bool_or, 2, CallByValue) in
    or_builtin := builtin;
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "^") (concat, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "to_string") (to_string, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "match1") (match1, 4, CallByName) in
    scope
  end
