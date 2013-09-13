(* traversal.ml: Node graph traversal and transformation implementation.

   Copyright (C) 2013 by Łukasz Czajka

*)

open Node

type 'a result_t = Skip of 'a | Continue of 'a

let traverse0 f node acc =
  let proxies = Stack.create ()
  in
  let cleanup () =
    Stack.iter (fun (r, x) -> r := x) proxies;
    Stack.clear proxies
  in
  let rec do_traverse f node acc =
    if node = Dummy then
      acc
    else
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
                  if !r = Dummy then
                    acc2
                  else
                    let x = !r
                    in
                    begin
                      Stack.push (r, x) proxies;
                      r := Dummy;
                      do_traverse f x acc2
                    end
              | Var(_) | MakeRecord(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
                True | False | Placeholder | Ignore | Nil | Tokens(_) ->
                  acc2
              | _ -> Debug.print (to_string node); failwith "unknown node"
          end
  in
  Utils.try_finally (fun () -> do_traverse f node acc) cleanup

let transform0 g f node0 =
  let proxies = Stack.create ()
  in
  let cleanup () =
    Stack.iter (fun (r, x) -> r := !x) proxies;
    Stack.clear proxies
  in
  let rec do_transform g f node0 =
    if node0 = Dummy then
      Dummy
    else
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
                  if !r = Dummy then
                    node
                  else
                    let x = ref !r
                    in
                    begin
                      Stack.push (r, x) proxies;
                      r := Dummy;
                      x := do_transform g f !x;
                      node
                    end
              | Var(_) | MakeRecord(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
                True | False | Placeholder | Ignore | Nil | Tokens(_) ->
                  f node
              | Dummy ->
                  node
              | _ -> Debug.print (to_string node); failwith "unknown node"
          end
  in
  Utils.try_finally (fun () -> do_transform g f node0) cleanup

let traverse f node acc =
  let rec aux node env env_len acc =
    traverse0
      (fun node acc ->
        match node with
        | Var(n) ->
            if n < env_len then
              begin
                let x = Env.nth env n
                in
                if x = Dummy then
                  Continue(acc)
                else
                  Skip(aux x env env_len acc)
              end
            else
              Continue(acc)
        | Lambda(body, frame, call_type, times_entered, attrs) ->
            begin
              assert (frame <= env_len);
              let env2 = Env.pop_n env (env_len - frame)
              in
              match f node acc with
              | Skip(acc2) -> Skip(acc2)
              | Continue(acc2) ->
                  Skip(aux body (Dummy :: env2) (frame + 1) acc2)
            end
        | BMatch(x, branches) ->
            let rec aux2 lst acc =
              match lst with
              | (x, y, z, n) :: t ->
                  let env2 = Env.push_n env Dummy n
                  and env2_len = env_len + n
                  in
                  aux2 t (aux z env2 env2_len
                            (aux y env2 env2_len
                               (aux x env env_len acc)))
              | [] -> Skip(acc)
            in
            aux2 branches (aux x env env_len acc)
        | Closure(x, env, env_len) ->
            Skip(aux x env env_len acc)
        | LambdaClosure(body, env, env_len, call_type, times_entered, attrs) ->
            Skip(aux body (Dummy :: env) (env_len + 1) acc)
        | _ ->
            Continue(acc))
      node
      acc
  in
  aux node [] 0 acc

let transform g f node =
  let rec aux node env env_len frames_num =
    transform0
      (fun node ->
        match g node frames_num with
        | Skip(node) -> Skip(node)
        | Continue(node) ->
            begin
              match node with
              | Var(n) ->
                  if n < env_len then
                    begin
                      let x = Env.nth env n
                      in
                      if x = Dummy then
                        Continue(node)
                      else
                        Skip(aux x env env_len frames_num)
                    end
                  else
                    Continue(node)
              | Lambda(body, frame, call_type, times_entered, attrs) ->
                  assert (frame <= env_len);
                  let env2 = Env.pop_n env (env_len - frame)
                  and env2_len = frame + 1
                  in
                  let rec cntfnum lst acc =
                    match lst with
                    | Dummy :: t -> cntfnum t (acc + 1)
                    | _ -> acc
                  in
                  let fnum = cntfnum env2 0
                  in
                  Skip(f (Lambda(aux body (Dummy :: env2) env2_len (fnum + 1), fnum, call_type, times_entered, attrs)))
              | BMatch(x, branches) ->
                  let rec aux2 lst acc =
                    match lst with
                    | (x, y, z, n) :: t ->
                        let env2 = Env.push_n env Dummy n
                        and env2_len = env_len + n
                        and fnum2 = frames_num + n
                        in
                        aux2 t ((aux x env env_len frames_num, aux y env2 env2_len fnum2, aux z env2 env2_len fnum2, n) :: acc)
                    | [] -> List.rev acc
                  in
                  Skip(f (BMatch(aux x env env_len frames_num, aux2 branches [])))
              | Closure(x, env2, env2_len) ->
                  Skip(aux x env2 env2_len 0)
              | LambdaClosure(body, env, env_len, call_type, times_entered, attrs) ->
                  Skip(aux (Lambda(body, 0, call_type, times_entered, attrs)) env env_len 0)
              | _ ->
                  Continue(node)
            end)
      f
      node
  in
  aux node [] 0 0
