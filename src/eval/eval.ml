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
      | [] -> assert false
    else
      env

let do_close x env env_len =
  match x with
  | Integer(_) | String(_) | Record(_) | Sym(_) | True | False |
    Placeholder | Ignore | Cons(_) | Nil | Quoted(_) | Closure(_) | Lambda(_, 0, _, _, _)
    -> x
  | Lambda(_, frame, _, _, _) -> Closure(x, Env.pop_n env (env_len - frame), frame)
  | Var(n) -> Env.nth env n
  | _ -> Closure(x, env, env_len)

let rec apply_lambda body frame call_type y a_env a_env_len env env_len =
  assert (env_len >= frame);
  let env2 = pop_to env env_len frame
  and env2_len = frame
  in
  if call_type = CallByValue then
    begin
      assert (env_len >= frame);
      let arg = do_eval y a_env a_env_len
      in
      do_eval body (arg :: env2) (env2_len + 1)
    end
  else
    begin
      match y with
      | Force(arg) ->
          do_eval body ((do_eval arg a_env a_env_len) :: env2) (env2_len + 1)
      | Leave(arg) ->
          do_eval body ((do_close arg a_env a_env_len) :: env2) (env2_len + 1)
      | _ ->
          let arg =
            if call_type = CallByNeed then
              Delayed(ref (do_close y a_env a_env_len))
            else
              do_close y a_env a_env_len
          in
          do_eval body (arg :: env2) (env2_len + 1)
    end

and do_eval node env env_len =
  match node with
  | Appl(x, y, attrs) ->
      begin
        let x = do_eval x env env_len
        in
        match x with
        | Lambda(body, frame, call_type, _, _) ->
            apply_lambda body frame call_type y env env_len env env_len
        | Closure(Lambda(body, frame, call_type, _, _), env2, env2_len) ->
            apply_lambda body frame call_type y env env_len env2 env2_len
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

  | Lambda(_) ->
      do_close node env env_len

  | Builtin(func, args_num, _) ->
      assert (args_num >= env_len);
      do_eval (func env) (Env.pop_n env args_num) (env_len - args_num)

let reduce node = do_eval node [] 0

let eval node = do_eval node [] 0

let eval_limited node limit = do_eval node [] 0

let eval_in node env = do_eval node env (Env.length env)
