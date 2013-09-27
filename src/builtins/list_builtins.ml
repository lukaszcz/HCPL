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

let rec do_find f lst default =
  match lst with
  | Cons(h, t) ->
      begin
        let h1 = Eval.eval h
        in
        let c = Eval.eval (Appl(f, h1, None))
        in
        match c with
        | True ->
            h1
        | False ->
            do_find f t default
        | _ ->
            let x = do_find f t default
            in
            if x == default then
              h1
            else
              Cond(c, h1, x, None)
      end
  | _ -> Eval.eval default

let rec do_mapfind f lst default =
  match lst with
  | Cons(h, t) ->
      begin
        let c = Eval.eval (Appl(f, h, None))
        in
        if Node.is_immediate c then
          begin
            if Node.fast_equal default c then
              do_mapfind f t default
            else
              c
          end
        else
          Cond(BEq(default, c), c, do_mapfind f t default, None)
      end
  | _ -> default

let rec do_rev2 lst acc =
  match lst with
  | Cons(h, t) -> do_rev2 t (Cons(do_rev h Nil, acc))
  | _ -> acc

let rec do_rev_split f lst acc acc2 =
  match lst with
  | Cons(h, t) ->
      begin
        let e = Eval.eval (Appl(f, h, None))
        in
        match e with
        | True -> do_rev_split f t (Cons(acc2, acc)) Nil
        | False -> do_rev_split f t acc (Cons(h, acc2))
        | _ -> Cond(e, do_rev_split f t (Cons(acc2, acc)) Nil, do_rev_split f t acc (Cons(h, acc2)), None)
      end
  | _ ->
      Cons(acc2, acc)

let do_split f lst = do_rev2 (do_rev_split f lst Nil Nil) Nil

let rec do_rev_split_n f n lst acc acc2 =
  if n = 0 then
    Cons(do_rev lst acc2, acc)
  else
    match lst with
    | Cons(h, t) ->
        begin
          let e = Eval.eval (Appl(f, h, None))
          in
          match e with
          | True -> do_rev_split_n f (n - 1) t (Cons(acc2, acc)) Nil
          | False -> do_rev_split_n f n t acc (Cons(h, acc2))
          | _ -> Cond(e, do_rev_split_n f (n - 1) t (Cons(acc2, acc)) Nil, do_rev_split_n f n t acc (Cons(h, acc2)), None)
        end
    | _ ->
        Cons(acc2, acc)

let do_split_n f n lst =
  match lst with
  | Cons(h, t) -> do_rev2 (do_rev_split_n f n lst Nil Nil) Nil
  | _ -> Cons(Nil, Nil)

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

let list_find lst =
  match lst with
  | y :: x :: f :: _ -> do_find f x y
  | _ -> assert false

let list_mapfind lst =
  match lst with
  | y :: x :: f :: _ ->
      if Node.is_fast_equal_pat y then
        do_mapfind f x y
      else
        Error.runtime_error "the third argument of List.mapfind must be a constant"
  | _ -> assert false

let list_split lst =
  match lst with
  | x :: f :: _ -> do_split f x
  | _ -> assert false

let list_split_n lst =
  match lst with
  | y :: f :: x :: _ -> do_split_n f (Bignum.to_int x) y
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
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_find") (list_find, 3, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_mapfind") (list_mapfind, 3, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_split") (list_split, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_list_split_n") (list_split_n, 3, CallByValue) in
    scope
  end
