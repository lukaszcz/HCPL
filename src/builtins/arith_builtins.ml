(* arith_builtins.ml: Basic arithmetic operation builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Big_int
open Node

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

let declare_builtins scope symtab =
  begin
    let scope = Builtin.declare scope (Symtab.find symtab "+") (add, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "-") (sub, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "*") (mul, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "div") (div, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "mod") (xmod, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab ">") (gt, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "<") (lt, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab ">=") (ge, 2, true) in
    let scope = Builtin.declare scope (Symtab.find symtab "<=") (le, 2, true) in
    scope
  end
