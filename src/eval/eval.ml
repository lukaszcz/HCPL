(* eval.ml: Evaluator implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

(*
   Invariants:
      - nodes stored in environments are closed
      - do_eval returns a closed node
      - quoted nodes (i.e. inside Quoted(_)) are closed // TODO: this is not an invariant
      - quoted nodes do not contain any Closure, LambdaClosure or
        Delayed nodes unless Config.is_unsafe_mode () returns true
*)

let pop_to env env_len frm =
  if frm = 0 then
    []
  else
    begin
      assert (frm <= env_len);
      let n = env_len - frm
      in
      if n > 0 then
        match env with
        | _ :: t -> Env.pop_n t (n - 1)
        | [] -> assert (env <> []); []
      else
        env
    end

let do_close x env env_len =
    match x with
    | Appl(_) | Cond(_) | Delay(_) | Leave(_) | Force(_) | Proxy(_) | MakeRecord(_) |
      BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
      BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_)
      ->
        Closure(x, env, env_len)
    | Lambda(body, frame, call_type, times_entered, attrs) ->
        LambdaClosure(body, Env.pop_n env (env_len - frame), frame, call_type, times_entered, attrs)
    | Var(n) -> assert (n < env_len); Env.nth env n
    | _ -> x

let rec do_delay x env env_len =
   match x with
   | Appl(_) | Cond(_) | Delay(_) | Leave(_) | Force(_) | MakeRecord(_) | Proxy(_) |
     BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
     BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_)
     ->
       Delayed(ref (Closure(x, env, env_len)))
   | Closure(_) -> Delayed(ref x)
   | Lambda(body, frame, call_type, times_entered, attrs) ->
       LambdaClosure(body, Env.pop_n env (env_len - frame), frame, call_type, times_entered, attrs)
   | Var(n) -> assert (n < env_len); assert (env <> []);
       do_delay (Env.nth env n) [] 0 (* passing [] is OK since values in envs are closed, so the env will not be needed *)
   | _ -> x

(* EVAL_DELAYED(r) *)
m4_define(`EVAL_DELAYED', `
  begin
    match !($1) with
    | Closure(x, env2, env2_len) ->
        begin
          $1 := do_eval x env2 env2_len;
          !($1)
        end
    | x ->
        begin
          assert (is_immed x || (match x with Lambda(_, 0, _, _, _) -> true | _ -> false));
          x
        end
  end
')

(* ACCESS_VAR(env, env_len, n) *)
m4_define(`ACCESS_VAR', `
  begin
    assert ($3 < $2);
    let x = Env.nth $1 $3
    in
    (* keep in mind that the values in environments are closed *)
    match x with
    | Closure(a, env2, env2_len) ->
        do_eval a env2 env2_len
    | Delayed(r) ->
        do_eval_delayed r
    | _ -> (*assert (is_immed x || (match x with Lambda(_, 0, _, _, _) -> true | _ -> false));*) x
           (* TODO: this need not be true, e.g.: let x = hd 3; This should be fixed!!! *)
  end
')

(* EVAL(node, env, env_len) *)
m4_define(`EVAL', `
  begin
    match $1 with
    | Var(n) ->
        ACCESS_VAR($2, $3, n)
    | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
      MakeRecord(_) | Closure(_) | Lambda(_) | Builtin(_) |
      BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
      BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_)
      -> do_eval $1 $2 $3
    | Integer(_) | String(_) | Record(_) | Sym(_) |
      True | False | Placeholder | Ignore | Cons(_) | Nil | Tokens(_) | Quoted(_) |
      LambdaClosure(_)| Unboxed1 | Unboxed2 | Unboxed3 | Unboxed4 | Unboxed5 | Unboxed6
      -> $1
  end
')

let dummy_env = ((Obj.magic 100) : Node.t list)

(* refstack and eval_limit are modified by eval, reduce, etc. *)
let refstack = Stack.create ()
let eval_limit = ref (-1)

let check_limit times_entered =
  if !eval_limit >= 0 then
    begin
      if !eval_limit > !times_entered then
        begin
          if !times_entered = 0 then
            begin
              Stack.push times_entered refstack
            end;
          incr times_entered;
          true
        end
      else
        false
    end
  else
    true

let rec do_eval_delayed r =
  EVAL_DELAYED(r)
and do_eval node env env_len =
  match node with
  | Appl(x, y, attrs) ->
      begin
        let x = do_eval x env env_len
        in
        match x with
        | Lambda(body, frame, CallByValue, times_entered, _) ->
            let arg = EVAL(y, env, env_len)
            in
            if check_limit times_entered then
              begin
                let env2 = pop_to env env_len frame
                and env2_len = frame
                in
                do_eval body (arg :: env2) (env2_len + 1)
              end
            else
              begin
                Appl(x, arg, attrs)
              end

        | Lambda(body, frame, call_type, times_entered, _) ->
            begin
              match y with
              | Force(arg) ->
                  let arg = do_eval arg env env_len
                  in
                  if check_limit times_entered then
                    let env2 = pop_to env env_len frame
                    and env2_len = frame
                    in
                    do_eval body (arg :: env2) (env2_len + 1)
                  else
                    Appl(x, arg, attrs)
              | Leave(arg) ->
                  let arg = do_close arg env env_len
                  in
                  if check_limit times_entered then
                    let env2 = pop_to env env_len frame
                    and env2_len = frame
                    in
                    do_eval body (arg :: env2) (env2_len + 1)
                  else
                    Appl(x, arg, attrs)
              | _ ->
                  let arg =
                    if call_type = CallByNeed then
                      do_delay y env env_len
                    else
                      do_close y env env_len
                  in
                  if check_limit times_entered then
                    let env2 = pop_to env env_len frame
                    and env2_len = frame
                    in
                    do_eval body (arg :: env2) (env2_len + 1)
                  else
                    Appl(x, arg, attrs)
            end

        | LambdaClosure(body, env2, env2_len, CallByValue, times_entered, _) ->
            let arg = EVAL(y, env, env_len)
            in
            if check_limit times_entered then
              do_eval body (arg :: env2) (env2_len + 1)
            else
              Appl(x, arg, attrs)

        | LambdaClosure(body, env2, env2_len, call_type, times_entered, _) ->
            begin
              match y with
              | Force(arg) ->
                  let arg = do_eval arg env env_len
                  in
                  if check_limit times_entered then
                    do_eval body (arg :: env2) (env2_len + 1)
                  else
                    Appl(x, arg, attrs)
              | Leave(arg) ->
                  let arg = do_close arg env env_len
                  in
                  if check_limit times_entered then
                    do_eval body (arg :: env2) (env2_len + 1)
                  else
                    Appl(x, arg, attrs)
              | _ ->
                  let arg =
                    if call_type = CallByNeed then
                      do_delay y env env_len
                    else
                      do_close y env env_len
                  in
                  if check_limit times_entered then
                    do_eval body (arg :: env2) (env2_len + 1)
                  else
                    Appl(x, arg, attrs)
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
      ACCESS_VAR(env, env_len, n)

  | Delayed(rx) ->
      EVAL_DELAYED(rx)

  | Proxy(rx) ->
      do_eval !rx env env_len

  | Closure(x, env2, env2_len) ->
      do_eval x env2 env2_len

  | Force(x) ->
      do_eval x env env_len

  | Delay(x) ->
      do_delay x env env_len

  | Leave(x) ->
      do_close x env env_len

  | MakeRecord(identtab) ->
      Record(Symbol.Map.map (fun x -> do_close x env env_len) identtab)

  | Lambda(_, 0, _, _, _) | LambdaClosure(_) -> node

  | Lambda(body, frame, call_type, times_entered, attrs) ->
      LambdaClosure(body, Env.pop_n env (env_len - frame), frame, call_type, times_entered, attrs)

  | Builtin(func, args_num, _) ->
      assert (args_num >= env_len);
      do_eval (func env) (Env.pop_n env args_num) (env_len - args_num)

  (* inlined builtins *)

  | BEq(x, y) ->
      let node1 = EVAL(x, env, env_len)
      in
      let node2 = EVAL(y, env, env_len)
      in
      if is_const node1 || is_const node2 then
        begin
          if node1 == node2 then
            True
          else
            False
        end
      else
        begin
          try
            if Match.equal node1 node2 then True else False
          with Match.Unknown ->
            BEq(node1, node2)
        end

  | BGt(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Bignum.gt x y
      end

  | BGe(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Bignum.ge x y
      end

  | BAdd(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Bignum.add x y
      end

  | BSub(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Bignum.sub x y
      end

  | BMul(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Bignum.mul x y
      end

  | BIDiv(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Bignum.idiv x y
      end

  | BMod(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Bignum.modulo x y
      end

  | BCons(x, y) ->
      begin
        let x = EVAL(x, env, env_len)
        in
        let y = EVAL(y, env, env_len)
        in
        Cons(x, y)
      end

  | BConsNE(x, y) ->
      Cons(do_close x env env_len, do_close y env env_len)

  | BFst(x) ->
      begin
        match x with
        | Var(n) ->
            begin
              let x = Env.nth env n
              in
              match x with
              | Cons(a, _) -> do_eval a env env_len
              | _ ->
                  begin
                    match do_eval x env env_len with
                    | Cons(a, _) -> do_eval a env env_len
                    | a -> BFst(a)
                  end
            end
        | _ ->
            begin
              match do_eval x env env_len with
              | Cons(a, _) -> do_eval a env env_len
              | a -> BFst(a)
            end
      end

  | BSnd(x) ->
      begin
        match x with
        | Var(n) ->
            begin
              let x = Env.nth env n
              in
              match x with
              | Cons(_, a) -> do_eval a env env_len
              | _ ->
                  begin
                    match do_eval x env env_len with
                    | Cons(_, a) -> do_eval a env env_len
                    | a -> BFst(a)
                  end
            end
        | _ ->
            begin
              match do_eval x env env_len with
              | Cons(_, a) -> do_eval a env env_len
              | a -> BSnd(a)
            end
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
        let x = do_eval x env env_len in
        let y = do_eval y env env_len in
        match x, y with
        | True, True -> True
        | False, _ -> False
        | _, False -> False
        | _ -> BAnd(x, y)
      end

  | BOr(x, y) ->
      begin
        let x = do_eval x env env_len in
        let y = do_eval y env env_len in
        match x, y with
        | False, False -> False
        | True, _ -> True
        | _, True -> True
        | _ -> BOr(x, y)
      end

  | BMatch(x, branches) ->
      let rec loop node env env_len lst =
        match lst with
        | (y, cond, body, args_num) :: t ->
            begin
              let pat = do_eval y env env_len
              in
              match pat with
              | Sym(psym) ->
                  begin
                    match node with
                    | Sym(nsym) when Symbol.eq psym nsym ->
                        assert (args_num = 0);
                        if cond == True || do_eval cond env env_len == True then
                          do_eval body env env_len
                        else
                          loop node env env_len t
                    | _ ->
                        loop node env env_len t
                  end
              | Cons(ph, pt) ->
                  begin
                    match node with
                    | Cons(nh, nt) ->
                        begin
                          match (ph, pt) with
                          | Placeholder, Placeholder ->
                              let env2 = nt :: nh :: env
                              and env2_len = env_len + 2
                              in
                              if cond == True || do_eval cond env2 env2_len == True then
                                do_eval body env2 env2_len
                              else
                                loop node env env_len t
                          | Placeholder, Ignore ->
                              let env2 = nh :: env
                              and env2_len = env_len + 1
                              in
                              if cond == True || do_eval cond env2 env2_len == True then
                                do_eval body env2 env2_len
                              else
                                loop node env env_len t
                          | Ignore, Placeholder ->
                              let env2 = nt :: env
                              and env2_len = env_len + 1
                              in
                              if cond == True || do_eval cond env2 env2_len == True then
                                do_eval body env2 env2_len
                              else
                                loop node env env_len t
                          | Ignore, Ignore ->
                              if cond == True || do_eval cond env env_len == True then
                                do_eval body env env_len
                              else
                                loop node env env_len t
                          | Placeholder, _ ->
                              begin
                                let env2 =
                                  try
                                    Match.xmatch nt pt (nh :: env)
                                  with Exit ->
                                    dummy_env
                                and env2_len = env_len + args_num
                                in
                                if env2 != dummy_env && (cond == True || do_eval cond env2 env2_len == True) then
                                  do_eval body env2 env2_len
                                else
                                  loop node env env_len t
                              end
                          | Ignore, _ ->
                              begin
                                let env2 =
                                  try
                                    Match.xmatch nt pt env
                                  with Exit ->
                                    dummy_env
                                and env2_len = env_len + args_num
                                in
                                if env2 != dummy_env && (cond == True || do_eval cond env2 env2_len == True) then
                                  do_eval body env2 env2_len
                                else
                                  loop node env env_len t
                              end
                          | _, Placeholder ->
                              begin
                                let env2 =
                                  try
                                    nt :: (Match.xmatch nh ph env)
                                  with Exit ->
                                    dummy_env
                                and env2_len = env_len + args_num
                                in
                                if env2 != dummy_env && (cond == True || do_eval cond env2 env2_len == True) then
                                  do_eval body env2 env2_len
                                else
                                  loop node env env_len t
                              end
                          | _, Ignore ->
                              begin
                                let env2 =
                                  try
                                    Match.xmatch nh ph env
                                  with Exit ->
                                    dummy_env
                                and env2_len = env_len + args_num
                                in
                                if env2 != dummy_env && (cond == True || do_eval cond env2 env2_len == True) then
                                  do_eval body env2 env2_len
                                else
                                  loop node env env_len t
                              end
                          | _ ->
                              begin
                                let env2 =
                                  try
                                    Match.xmatch nt pt (Match.xmatch nh ph env)
                                  with Exit ->
                                    dummy_env
                                and env2_len = env_len + args_num
                                in
                                if env2 != dummy_env && (cond == True || do_eval cond env2 env2_len == True) then
                                  do_eval body env2 env2_len
                                else
                                  loop node env env_len t
                              end
                        end
                    | _ ->
                        loop node env env_len t
                  end
              | Appl(_) | Cond(_) | Delay(_) | Leave(_) | Force(_) | Var(_) | Proxy(_) | MakeRecord(_) |
                BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
                BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
                Closure(_) | Delayed(_) | Lambda(_) | Builtin(_) | Integer(_) | String(_) |
                Record(_) | Tokens(_) | Quoted(_) | LambdaClosure(_)
                ->
                  begin
                    let env2 =
                      try
                        Match.xmatch node pat env
                      with Exit ->
                        dummy_env
                    and env2_len = env_len + args_num
                    in
                    if env2 != dummy_env && (cond == True || do_eval cond env2 env2_len == True) then
                      do_eval body env2 env2_len
                    else
                      loop node env env_len t
                  end
              | _ ->
                  begin
                    if pat == Ignore then
                      begin
                        assert (args_num = 0);
                        if cond == True || do_eval cond env env_len == True then
                          do_eval body env env_len
                        else
                          loop node env env_len t
                      end
                    else if pat == Placeholder then
                      begin
                        assert (args_num = 1);
                        let env2 = node :: env
                        and env2_len = env_len + 1
                        in
                        if cond == True || do_eval cond env2 env2_len == True then
                          do_eval body env2 env2_len
                        else
                          loop node env env_len t
                      end
                    else if pat == node then
                      begin
                        assert (args_num = 0);
                        if cond == True || do_eval cond env env_len == True then
                          do_eval body env env_len
                        else
                          loop node env env_len t
                      end
                    else
                      loop node env env_len t
                  end
            end
        | [] -> Error.runtime_error "match failure"
      in
      loop (do_eval x env env_len) env env_len branches

  | BRecordGet(x, y) ->
      begin
        let x = do_eval x env env_len in
        let y = do_eval y env env_len in
        match x, y with
        | Record(r), Sym(s) -> Symbol.Map.find s r
        | _ -> Error.runtime_error "record error"
      end

  | _ -> node

let eval node =
  let prev_limit = !eval_limit
  in
  let cleanup () =
    eval_limit := prev_limit;
  in
  eval_limit := -1;
  Utils.try_finally (fun () -> do_eval node Env.empty 0) cleanup

let eval_limited node limit =
  let prev_limit = !eval_limit
  in
  let cleanup () =
    Stack.iter (fun x -> x := 0) refstack;
    Stack.clear refstack;
    eval_limit := prev_limit;
  in
  eval_limit := limit;
  Utils.try_finally (fun () -> do_eval node Env.empty 0) cleanup

let reduce node = eval_limited node 1

let eval_in node env = eval_limit := -1; do_eval node env (Env.length env)

let macro_tmp_id = ref 0
let extra_macro_args_ref = ref []

let eval_macro symtab node args =
  let n = 10
  in
  let rec mkextra m acc =
    if m = 0 then
      acc
    else
      let sym = Symtab.find symtab ("__ipl_macro_tmp_" ^ string_of_int !macro_tmp_id)
      in
      incr macro_tmp_id;
      mkextra (m - 1) (Node.Tokens([(Token.Symbol(sym), Lexing.dummy_pos)]) :: acc)
  in
  let arglst = List.fold_right (fun x acc -> Node.Cons(x, acc)) args Node.Nil
  in
  let mcall = Node.Appl(node, arglst, None)
  in
  extra_macro_args_ref := mkextra n [];
  eval mcall

let extra_macro_args () = !extra_macro_args_ref
