(* eval.ml: Evaluator implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

let pop_to env env_len frm =
  if frm = 0 then
    []
  else
    let n = env_len - frm
    in
    if n > 0 then
      match env with
      | h :: t -> Env.pop_n t (n - 1)
      | [] -> assert (env <> []); []
    else
      env

let do_close x env env_len =
  match x with
  | Integer(_) | String(_) | Record(_) | Sym(_) | True | False |
    Placeholder | Ignore | Cons(_) | Nil | Quoted(_) | Closure(_) |
    Lambda(_, 0, _, _, _) | LambdaEager(_, 0, _, _) |
    LambdaClosure(_) | LambdaEagerClosure(_)
    -> x
  | Lambda(body, frame, call_type, times_entered, attrs) ->
      LambdaClosure(body, Env.pop_n env (env_len - frame), frame, call_type, times_entered, attrs)
  | LambdaEager(body, frame, times_entered, attrs) ->
      LambdaEagerClosure(body, Env.pop_n env (env_len - frame), frame, times_entered, attrs)
  | Var(n) -> assert (n < env_len); Env.nth env n
  | _ -> Closure(x, env, env_len)

let rec do_eval node env env_len =
  match node with
  | Appl(x, y, attrs) ->
      begin
        let x = do_eval x env env_len
        in
        match x with
        | LambdaEager(body, frame, _, _) | Lambda(body, frame, CallByValue, _, _) ->
            let arg = do_eval y env env_len
            and env2 = pop_to env env_len frame
            and env2_len = frame
            in
            do_eval body (arg :: env2) (env2_len + 1)

        | Lambda(body, frame, call_type, _, _) ->
            begin
              match y with
              | Force(arg) ->
                  let arg = do_eval arg env env_len
                  and env2 = pop_to env env_len frame
                  and env2_len = frame
                  in
                  do_eval body (arg :: env2) (env2_len + 1)
              | Leave(arg) ->
                  let arg = do_close arg env env_len
                  and env2 = pop_to env env_len frame
                  and env2_len = frame
                  in
                  do_eval body (arg :: env2) (env2_len + 1)
              | _ ->
                  let arg =
                    if call_type = CallByNeed then
                      Delayed(ref (do_close y env env_len))
                    else
                      do_close y env env_len
                  and env2 = pop_to env env_len frame
                  and env2_len = frame
                  in
                  do_eval body (arg :: env2) (env2_len + 1)
            end

        | LambdaEagerClosure(body, env2, env2_len, _, _) | LambdaClosure(body, env2, env2_len, CallByValue, _, _) ->
            let arg = do_eval y env env_len
            in
            do_eval body (arg :: env2) (env2_len + 1)

        | LambdaClosure(body, env2, env2_len, call_type, _, _) ->
            begin
              match y with
              | Force(arg) ->
                  let arg = do_eval arg env env_len
                  in
                  do_eval body (arg :: env2) (env2_len + 1)
              | Leave(arg) ->
                  let arg = do_close arg env env_len
                  in
                  do_eval body (arg :: env2) (env2_len + 1)
              | _ ->
                  let arg =
                    if call_type = CallByNeed then
                      Delayed(ref (do_close y env env_len))
                    else
                      do_close y env env_len
                  in
                  do_eval body (arg :: env2) (env2_len + 1)
            end

        | _ -> Appl(x, do_close y env env_len, attrs)
      end

  | Cond(x, y, z, attrs) ->
      begin
        let x1 = do_eval x env env_len
        in
        match x1 with
        | True -> do_eval y env env_len
        | False -> do_eval z env env_len
        | _ -> Cond(x1, do_close y env env_len, do_close z env env_len, attrs)
      end

  | Var(n) ->
      assert (n < env_len);
      do_eval (Env.nth env n) env env_len

  | Delayed(rx) ->
      rx := do_eval !rx env env_len;
      !rx

  | Proxy(rx) ->
      do_eval !rx env env_len

  | Closure(x, env2, env2_len) ->
      do_eval x env2 env2_len

  | Force(x) ->
      do_eval x env env_len

  | Delay(x) ->
      Delayed(ref (do_close x env env_len))

  | Leave(x) ->
      do_close x env env_len

  | MakeRecord(identtab) ->
      Record(Symbol.Map.map (fun x -> do_close x env env_len) identtab)

  | Integer(_) | String(_) | Record(_) | Sym(_) | True | False | Placeholder | Ignore | Cons(_) | Nil | Quoted(_) ->
      node

  | Lambda(_, 0, _, _, _) | LambdaEager(_, 0, _, _) | LambdaClosure(_) | LambdaEagerClosure(_) -> node

  | Lambda(body, frame, call_type, times_entered, attrs) ->
      LambdaClosure(body, Env.pop_n env (env_len - frame), frame, call_type, times_entered, attrs)

  | LambdaEager(body, frame, times_entered, attrs) ->
      LambdaEagerClosure(body, Env.pop_n env (env_len - frame), frame, times_entered, attrs)

  | Builtin(func, args_num, _) ->
      assert (args_num >= env_len);
      do_eval (func env) (Env.pop_n env args_num) (env_len - args_num)

  (* inlined builtins *)

  | BEq(x, y) ->
      let x = do_eval x env env_len
      and y = do_eval y env env_len
      in
      let ret = Node.equal x y
      in
      if ret <> Nil then
        ret
      else
        BEq(x, y)

  | BGt(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Integer(a), Integer(b) -> if Big_int.gt_big_int a b then True else False
        | _ -> assert (1 = 0); False
      end

  | BGe(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Integer(a), Integer(b) -> if Big_int.ge_big_int a b then True else False
        | _ -> assert (1 = 0); False
      end

  | BAdd(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Integer(a), Integer(b) -> Integer(Big_int.add_big_int a b)
        | _ -> assert (1 = 0); False
      end

  | BSub(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Integer(a), Integer(b) -> Integer(Big_int.sub_big_int a b)
        | _ -> assert (1 = 0); False
      end

  | BMul(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Integer(a), Integer(b) -> Integer(Big_int.mult_big_int a b)
        | _ -> assert (1 = 0); False
      end

  | BIDiv(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Integer(a), Integer(b) -> Integer(Big_int.div_big_int a b)
        | _ -> assert (1 = 0); False
      end

  | BMod(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Integer(a), Integer(b) -> Integer(Big_int.mod_big_int a b)
        | _ -> assert (1 = 0); False
      end

  | BCons(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        Cons(x, y)
      end

  | BConsNE(x, y) -> Cons(do_close x env env_len, do_close y env env_len)

  | BFst(x) ->
      begin
        match do_eval x env env_len with
        | Cons(a, _) -> do_eval a env env_len
        | a -> BFst(a)
      end

  | BSnd(x) ->
      begin
        match do_eval x env env_len with
        | Cons(_, a) -> do_eval a env env_len
        | a -> BSnd(a)
      end

  | BNot(x) ->
      begin
        let x = do_eval x env env_len
        in
        match x with
        | True -> False
        | False -> True
        | _ -> BNot(x)
      end

  | BAnd(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | True, True -> True
        | False, _ -> False
        | _, False -> False
        | _ -> BAnd(x, y)
      end

  | BOr(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | False, False -> False
        | True, _ -> True
        | _, True -> True
        | _ -> BOr(x, y)
      end

  | BMatch1(x, y, z1, z2) ->
      let node = do_eval x env env_len
      and pat = do_eval y env env_len
      in
      let (m, args) = Node.matches node pat
      in
      if m then
        if args = [] then
          do_eval z1 env env_len
        else
          do_eval (Node.mkappl (z1 :: args) None) env env_len
      else
        do_eval z2 env env_len

  | BRecordGet(x, y) ->
      begin
        let x = do_eval x env env_len
        and y = do_eval y env env_len
        in
        match x, y with
        | Record(r), Sym(s) -> Symbol.Map.find s r
        | _ -> failwith "record error"
      end

let reduce node = do_eval node [] 0

let eval node = do_eval node [] 0

let eval_limited node limit = do_eval node [] 0

let eval_in node env = do_eval node env (Env.length env)
