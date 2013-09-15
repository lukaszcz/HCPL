(* list_builtins.ml: List builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node

let rec do_length lst acc =
  match lst with
  | Cons(_, t) -> do_length t (acc + 1)
  | _ -> acc

let rec do_nth n lst =
  match lst with
  | Cons(h, t) ->
      if n = 0 then
        h
      else
        do_nth (n - 1) t
  | _ -> Error.runtime_error "List.nth: bad index"

let rec do_take n lst =
  if n = 0 then
    Nil
  else
    match lst with
    | Cons(h, t) -> Cons(h, do_take (n - 1) t)
    | _ -> Nil

let rec do_put n elem lst =
  if n = 0 then
    lst
  else
    do_put (n - 1) elem (Cons(elem, lst))

let rec do_tail n lst =
  if n <= 0 then
    lst
  else
    match lst with
    | Cons(_, t) -> do_tail (n - 1) t
    | _ -> Nil

let rec do_rev lst acc =
  match lst with
  | Cons(h, t) -> do_rev t (Cons(h, acc))
  | _ -> acc

let rec do_append x y =
  match x with
  | Cons(h, t) -> Cons(h, do_append t y)
  | _ -> y

let rec do_rev_append x y =
  match x with
  | Cons(h, t) -> do_rev_append t (Cons(h, y))
  | _ -> y

let rec do_map f lst =
  match lst with
  | Cons(h, t) -> Cons(Eval.eval (Appl(f, h, None)), do_map f t)
  | _ -> Nil

let rec do_rev_map f lst acc =
  match lst with
  | Cons(h, t) -> do_rev_map f t (Cons(Eval.eval (Appl(f, h, None)), acc))
  | _ -> acc

let rec do_filter f lst =
  match lst with
  | Cons(h, t) ->
      begin
        let c = Eval.eval (Appl(f, h, None))
        in
        match c with
        | True ->
            Cons(h, do_filter f t)
        | False ->
            do_filter f t
        | _ ->
            Appl(Lambda(Cond(c, BCons(h, Var(0)), Var(0), None), 0, CallByValue, ref 0, None), do_filter f t, None)
      end
  | _ -> Nil

let rec do_rev_filter f lst acc =
  match lst with
  | Cons(h, t) ->
      begin
        let c = Eval.eval (Appl(f, h, None))
        in
        match c with
        | True ->
            do_rev_filter f t (Cons(h, acc))
        | False ->
            do_rev_filter f t acc
        | _ ->
            do_rev_filter f t (Appl(Lambda(Cond(c, BCons(h, Var(0)), Var(0), None), 0, CallByValue, ref 0, None), acc, None))
      end
  | _ -> acc

let rec do_foldl f lst acc =
  match lst with
  | Cons(h, t) ->
      do_foldl f t (Eval.eval (Appl(Appl(f, h, None), acc, None)))
  | _ -> acc

let rec do_foldr f lst acc =
  match lst with
  | Cons(h, t) ->
      Eval.eval (Appl(Appl(f, h, None), do_foldr f t acc, None))
  | _ -> acc

let rec do_forall f lst =
  match lst with
  | Cons(h, t) ->
      begin
        let c = Eval.eval (Appl(f, h, None))
        in
        match c with
        | True ->
            do_forall f t
        | False ->
            False
        | _ ->
            let x = do_forall f t
            in
            if x = False then
              False
            else
              Cond(c, x, False, None)
      end
  | _ -> True

let rec do_exists f lst =
  match lst with
  | Cons(h, t) ->
      begin
        let c = Eval.eval (Appl(f, h, None))
        in
        match c with
        | True ->
            True
        | False ->
            do_exists f t
        | _ ->
            let x = do_exists f t
            in
            if x = True then
              True
            else
              Cond(c, True, x, None)
      end
  | _ -> False


(***********************************************************************)

let list_length lst =
  match lst with
  | x :: _ -> Bignum.from_int (do_length x 0)
  | _ -> assert false

let list_nth lst =
  match lst with
  | y :: x :: _ -> do_nth (Bignum.to_int y) x
  | _ -> assert false

let list_head lst =
  match lst with
  | y :: x :: _ -> do_take (Bignum.to_int y) x
  | _ -> assert false

let list_tail lst =
  match lst with
  | y :: x :: _ -> do_tail (do_length x 0 - (Bignum.to_int y)) x
  | _ -> assert false

let list_put lst =
  match lst with
  | z :: y :: x :: _ -> do_put (Bignum.to_int x) y z
  | _ -> assert false

let list_rev lst =
  match lst with
  | x :: _ -> do_rev x Nil
  | _ -> assert false

let list_append lst =
  match lst with
  | y :: x :: _ -> do_append x y
  | _ -> assert false

let list_rev_append lst =
  match lst with
  | y :: x :: _ -> do_rev_append x y
  | _ -> assert false

let list_map lst =
  match lst with
  | x :: f :: _ -> do_map f x
  | _ -> assert false

let list_rev_map lst =
  match lst with
  | x :: f :: _ -> do_rev_map f x Nil
  | _ -> assert false

let list_filter lst =
  match lst with
  | x :: f :: _ -> do_filter f x
  | _ -> assert false

let list_rev_filter lst =
  match lst with
  | x :: f :: _ -> do_rev_filter f x Nil
  | _ -> assert false

let list_foldl lst =
  match lst with
  | y :: x :: f :: _ -> do_foldl f x y
  | _ -> assert false

let list_foldr lst =
  match lst with
  | y :: x :: f :: _ -> do_foldr f x y
  | _ -> assert false

let list_forall lst =
  match lst with
  | x :: f :: _ -> do_forall f x
  | _ -> assert false

let list_exists lst =
  match lst with
  | x :: f :: _ -> do_exists f x
  | _ -> assert false


(* public interface *)

let declare_builtins scope symtab =
  begin
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_length") (list_length, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_nth") (list_nth, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_head") (list_head, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_tail") (list_tail, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_put") (list_put, 3, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_rev") (list_rev, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_append") (list_append, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_rev_append") (list_rev_append, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_map") (list_map, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_rev_map") (list_rev_map, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_filter") (list_filter, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_rev_filter") (list_rev_filter, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_foldl") (list_foldl, 3, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_foldr") (list_foldr, 3, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_forall") (list_forall, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_exists") (list_exists, 2, CallByValue) in
    scope
  end
