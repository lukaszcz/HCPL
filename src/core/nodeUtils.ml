(* nodeUtils.ml: Node utilities implementation. Node graph traversal and transformation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node

type 'a result_t = Skip of 'a | Continue of 'a

let traverse f node acc =
  let proxies = Stack.create ()
  and dummy = Proxy(ref Nil)
  in
  let cleanup () =
    Stack.iter (fun (r, x) -> r := x) proxies;
    Stack.clear proxies
  in
  let rec do_traverse f node acc =
    match f node acc with
    | Skip(acc2) -> acc2
    | Continue(acc2) ->
        begin
          if is_smallint node then
            acc2
          else
            match node with
            | Appl(a, b, _) ->
                do_traverse f b (do_traverse f a acc2)
            | Cond(x, y, z, _) ->
                do_traverse f z (do_traverse f y (do_traverse f x acc2))
            | Delay(x) ->
                do_traverse f x acc2
            | Force(x) ->
                do_traverse f x acc2
            | Leave(x) ->
                do_traverse f x acc2
            | Lambda(body, _, _, _, _) ->
                do_traverse f body acc2
            | Quoted(x) ->
                do_traverse f x acc2
            | Cons(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | Delayed(x) ->
                do_traverse f !x acc2
            | Closure(body, _, _) ->
                do_traverse f body acc2
            | LambdaClosure(body, _, _, _, _, _) ->
                do_traverse f body acc2
            | BEq(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BGt(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BGe(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BAdd(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BSub(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BMul(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BIDiv(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BMod(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BCons(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BConsNE(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BFst(x) ->
                do_traverse f x acc2
            | BSnd(x) ->
                do_traverse f x acc2
            | BNot(x) ->
                do_traverse f x acc2
            | BAnd(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BOr(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | BMatch(x, branches) ->
                let rec aux lst acc =
                  match lst with
                  | (x, y, z, _) :: t ->
                      aux t (do_traverse f z (do_traverse f y (do_traverse f x acc)))
                  | [] ->
                      acc
                in
                aux branches (do_traverse f x acc2)
            | BRecordGet(x, y) ->
                do_traverse f y (do_traverse f x acc2)
            | Proxy(r) ->
                if !r == dummy then
                  acc2
                else
                  let x = !r
                  in
                  begin
                    Stack.push (r, x) proxies;
                    r := dummy;
                    do_traverse f x acc2
                  end
            | Var(_) | MakeRecord(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
              True | False | Placeholder | Ignore | Nil | Tokens(_) ->
                acc2
            | _ -> Debug.print (to_string node); failwith "unknown node"
        end
  in
  try
    let ret = do_traverse f node acc
    in
    cleanup ();
    ret
  with
    e ->
      cleanup ();
      raise e

let transform g f node0 =
  let proxies = Stack.create ()
  and dummy = Proxy(ref Nil)
  in
  let cleanup () =
    Stack.iter (fun (r, x) -> r := !x) proxies;
    Stack.clear proxies
  in
  let rec do_transform g f node0 =
    match g node0 with
    | Skip(node) -> node
    | Continue(node) ->
        begin
          if is_smallint node then
            f node
          else
            match node with
            | Appl(a, b, attrs) ->
                f (Appl(do_transform g f a, do_transform g f b, attrs))
            | Cond(x, y, z, attrs) ->
                f (Cond(do_transform g f x, do_transform g f y, do_transform g f z, attrs))
            | Delay(x) ->
                f (Delay(do_transform g f x))
            | Force(x) ->
                f (Force(do_transform g f x))
            | Leave(x) ->
                f (Leave(do_transform g f x))
            | Lambda(body, frame, call_type, _, attrs) ->
                f (Lambda(do_transform g f body, frame, call_type, ref 0, attrs))
            | Quoted(x) ->
                f (Quoted(do_transform g f x))
            | Cons(x, y) ->
                f (Cons(do_transform g f x, do_transform g f y))
            | Delayed(x) ->
                f (Delayed(ref (do_transform g f !x)))
            | Closure(body, env2, env2_len) ->
                f (Closure(do_transform g f body, env2, env2_len))
            | LambdaClosure(body, env, env_len, call_type, _, attrs) ->
                f (LambdaClosure(do_transform g f body, env, env_len, call_type, ref 0, attrs))
            | BEq(x, y) ->
                f (BEq(do_transform g f x, do_transform g f y))
            | BGt(x, y) ->
                f (BGt(do_transform g f x, do_transform g f y))
            | BGe(x, y) ->
                f (BGe(do_transform g f x, do_transform g f y))
            | BAdd(x, y) ->
                f (BAdd(do_transform g f x, do_transform g f y))
            | BSub(x, y) ->
                f (BSub(do_transform g f x, do_transform g f y))
            | BMul(x, y) ->
                f (BMul(do_transform g f x, do_transform g f y))
            | BIDiv(x, y) ->
                f (BIDiv(do_transform g f x, do_transform g f y))
            | BMod(x, y) ->
                f (BMod(do_transform g f x, do_transform g f y))
            | BCons(x, y) ->
                f (BCons(do_transform g f x, do_transform g f y))
            | BConsNE(x, y) ->
                f (BConsNE(do_transform g f x, do_transform g f y))
            | BFst(x) ->
                f (BFst(do_transform g f x))
            | BSnd(x) ->
                f (BSnd(do_transform g f x))
            | BNot(x) ->
                f (BNot(do_transform g f x))
            | BAnd(x, y) ->
                f (BAnd(do_transform g f x, do_transform g f y))
            | BOr(x, y) ->
                f (BOr(do_transform g f x, do_transform g f y))
            | BMatch(x, branches) ->
                let rec aux lst acc =
                  match lst with
                  | (x, y, z, n) :: t ->
                      aux t ((do_transform g f x, do_transform g f y, do_transform g f z, n) :: acc)
                  | [] ->
                      acc
                in
                f (BMatch(do_transform g f x, List.rev (aux branches [])))
            | BRecordGet(x, y) ->
                f (BRecordGet(do_transform g f x, do_transform g f y))
            | Proxy(r) ->
                if !r == dummy then
                  node
                else
                  let x = ref !r
                  in
                  begin
                    Stack.push (r, x) proxies;
                    r := dummy;
                    x := do_transform g f !x;
                    node
                  end
            | Var(_) | MakeRecord(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
              True | False | Placeholder | Ignore | Nil | Tokens(_) ->
                f node
            | _ -> Debug.print (to_string node); failwith "unknown node"
        end
  in
  try
    let ret = do_transform g f node0
    in
    cleanup ();
    ret
  with e ->
    cleanup ();
    raise e
