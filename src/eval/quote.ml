(* quote.ml: Quoting implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

let quote node =
  let rec do_quote_fetched node env env_len =
    match node with
    | Quoted(x) -> x
    | Integer(_) | String(_) | Record(_) | Sym(_) ->
        node
    | Cons(a, b) ->
        if Config.is_unsafe_mode () then
          node
        else
          Cons(do_quote_fetched a env env_len, do_quote_fetched b env env_len)
    | _ ->
        (* NOTE: the following check is necessary to ensure consistency of the logic *)
        if Config.is_unsafe_mode () then
          do_quote node env env_len
        else if is_const node then
          node
        else
          failwith "cannot quote a non-constant value"

  and do_quote node env env_len =
    match node with
    | Var(n) ->
        begin
          if n >= env_len then
            Var(n)
          else
            let x = Env.nth env n
            in
            match x with
            | Var(m) when m = n -> x
            | _ -> do_quote_fetched x env env_len
        end
    | Proxy(rx) -> !rx
    | Appl(x, y, attrs) ->
        Appl(do_quote x env env_len, do_quote y env env_len, attrs)
    | Cond(x, y, z, attrs) ->
        Cond(do_quote x env env_len, do_quote y env env_len, do_quote z env env_len, attrs)
    | Delay(x) ->
        Delay(do_quote x env env_len)
    | Leave(x) ->
        Leave(do_quote x env env_len)
    | Force(x) ->
        Force(do_quote x env env_len)
    | MakeRecord(_) ->
        node
    | BEq(x, y) ->
        BEq(do_quote x env env_len, do_quote y env env_len)
    | BGt(x, y) ->
        BGt(do_quote x env env_len, do_quote y env env_len)
    | BGe(x, y) ->
        BGe(do_quote x env env_len, do_quote y env env_len)
    | BAdd(x, y) ->
        BAdd(do_quote x env env_len, do_quote y env env_len)
    | BSub(x, y) ->
        BSub(do_quote x env env_len, do_quote y env env_len)
    | BMul(x, y) ->
        BMul(do_quote x env env_len, do_quote y env env_len)
    | BIDiv(x, y) ->
        BIDiv(do_quote x env env_len, do_quote y env env_len)
    | BMod(x, y) ->
        BMod(do_quote x env env_len, do_quote y env env_len)
    | BCons(x, y) ->
        BCons(do_quote x env env_len, do_quote y env env_len)
    | BConsNE(x, y) ->
        BConsNE(do_quote x env env_len, do_quote y env env_len)
    | BFst(x) ->
        BFst(do_quote x env env_len)
    | BSnd(x) ->
        BSnd(do_quote x env env_len)
    | BNot(x) ->
        BNot(do_quote x env env_len)
    | BAnd(x, y) ->
        BAnd(do_quote x env env_len, do_quote y env env_len)
    | BOr(x, y) ->
        BOr(do_quote x env env_len, do_quote y env env_len)
    | BMatch(x, lst) ->
        BMatch(do_quote x env env_len,
               List.fold_right
                 (fun (x, y, n) acc ->
                   (do_quote x env env_len, do_quote y env env_len, n) :: acc)
                 lst [])
    | BRecordGet(x, y) ->
        BRecordGet(do_quote x env env_len, do_quote y env env_len)
    | Closure(x, env2, env2_len) ->
        do_quote x env2 env2_len
    | Delayed(r) ->
        assert (Config.is_unsafe_mode ());
        Delayed(ref (do_quote !r env env_len));
    | Lambda(body, frame, call_type, seen, attrs) ->
        let env2 = Env.pop_n env (env_len - frame)
        and env2_len = frame
        in
        Lambda(do_quote body (Var(frame) :: env2) (env2_len + 1), frame, call_type, seen, attrs)
    | Quoted(x) -> x
    | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) ->
        node
    | LambdaClosure(body, env2, env2_len, call_type, seen, attrs) ->
        Lambda(do_quote body (Var(env2_len) :: env2) (env2_len + 1), env2_len, call_type, seen, attrs)
    | Cons(x, y) ->
        Cons(do_quote x env env_len, do_quote y env env_len)
    | _ -> node
  in
  Node.mkquoted (do_quote node [] 0)
