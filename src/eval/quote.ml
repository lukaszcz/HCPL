(* quote.ml: Quoting implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

let quote node =
  let rec do_quote_fetched node env env_len env_gap =
    match node with
    | Quoted(x) -> x
    | Integer(_) | String(_) | Record(_) | Sym(_) ->
        node
    | Cons(a, b) ->
        if Config.is_unsafe_mode () then
          node
        else
          Cons(do_quote_fetched a env env_len env_gap, do_quote_fetched b env env_len env_gap)
    | _ ->
        (* NOTE: the following check is necessary to ensure consistency of the logic *)
        if Config.is_unsafe_mode () then
          do_quote node env env_len env_gap
        else if is_const node then
          node
        else
          failwith "cannot quote a non-constant value"

  and do_quote node env env_len env_gap =
    match node with
    | Var(n) ->
        begin
          if n < env_gap then
            Var(n)
          else
            do_quote_fetched (Env.nth env (n - env_gap)) env env_len env_gap
        end
    | Proxy(rx) -> !rx
    | Appl(x, y, attrs) ->
        Appl(do_quote x env env_len env_gap, do_quote y env env_len env_gap, attrs)
    | Cond(x, y, z, attrs) ->
        Cond(do_quote x env env_len env_gap, do_quote y env env_len env_gap, do_quote z env env_len env_gap, attrs)
    | Delay(x) ->
        Delay(do_quote x env env_len env_gap)
    | Leave(x) ->
        Leave(do_quote x env env_len env_gap)
    | Force(x) ->
        Force(do_quote x env env_len env_gap)
    | MakeRecord(_) ->
        node
    | BEq(x, y) ->
        BEq(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BGt(x, y) ->
        BGt(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BGe(x, y) ->
        BGe(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BAdd(x, y) ->
        BAdd(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BSub(x, y) ->
        BSub(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BMul(x, y) ->
        BMul(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BIDiv(x, y) ->
        BIDiv(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BMod(x, y) ->
        BMod(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BCons(x, y) ->
        BCons(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BConsNE(x, y) ->
        BConsNE(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BFst(x) ->
        BFst(do_quote x env env_len env_gap)
    | BSnd(x) ->
        BSnd(do_quote x env env_len env_gap)
    | BNot(x) ->
        BNot(do_quote x env env_len env_gap)
    | BAnd(x, y) ->
        BAnd(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BOr(x, y) ->
        BOr(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | BMatch(x, lst) ->
        BMatch(do_quote x env env_len env_gap,
               List.fold_right
                 (fun (x, y, z, n) acc ->
                   (do_quote x env env_len env_gap,
                    do_quote y env (env_len + n) (env_gap + n),
                    do_quote z env (env_len + n) (env_gap + n), n) :: acc)
                 lst [])
    | BRecordGet(x, y) ->
        BRecordGet(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | Closure(x, env2, env2_len) ->
        do_quote x env2 env2_len 0
    | Delayed(r) ->
        assert (Config.is_unsafe_mode ());
        Delayed(ref (do_quote !r env env_len env_gap))
    | Lambda(body, frame, call_type, seen, attrs) ->
        let env2_gap = if frame >= env_len then frame - env_len else 0
        in
        let env2 = if env2_gap > 0 then env else Env.pop_n env (env_len - frame)
        and env2_len = if env2_gap > 0 then env_len else frame
        in
        Lambda(do_quote body env2 env2_len (env2_gap + 1), frame, call_type, seen, attrs)
    | Quoted(x) -> x
    | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) ->
        node
    | LambdaClosure(body, env2, env2_len, call_type, seen, attrs) ->
        Lambda(do_quote body env2 env2_len 1, env2_len, call_type, seen, attrs)
    | Cons(x, y) ->
        Cons(do_quote x env env_len env_gap, do_quote y env env_len env_gap)
    | _ -> node
  in
  Node.mkquoted (do_quote node [] 0 0)

let occurs_check node1 node2 =
  match node1 with
  | Quoted(a) ->
      begin
        try
          NodeUtils.traverse
            (fun x _ ->
              try
                if Match.equal_quoted x a then
                  raise Exit
                else
                  NodeUtils.Continue(false)
              with
                Match.Unknown -> NodeUtils.Continue(false)
            ) node2 false
        with Exit ->
          true
      end
  | _ ->
      failwith "expected a quoted value as the first argument of occurs-check"
