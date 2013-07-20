(* eval.ml: Evaluator implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

exception Unknown

let rec do_match_quoted node pat acc eq_mode =
  let node = Node.normalize node in
  let pat = Node.normalize pat in
  begin
    if is_smallint pat then
      begin
        if pat == node then
          acc
        else
          raise Exit
      end
    else if eq_mode && not (is_immediate node && is_immediate pat) then
      raise Unknown
    else
      match pat with
      | Appl(x, y, _) ->
          begin
            match node with
            | Appl(a, b, _) -> do_match_quoted b y (do_match_quoted a x acc eq_mode) eq_mode
            | _ -> raise Exit
          end
      | Cond(x, y, z, _) ->
          begin
            match node with
            | Cond(a, b, c, _) ->
                do_match_quoted c z (do_match_quoted b y (do_match_quoted a x acc eq_mode) eq_mode) eq_mode
            | _ ->
                raise Exit
          end
      | Delay(x) ->
          begin
            match node with
            | Delay(a) ->
                do_match_quoted a x acc eq_mode
            | _ ->
                raise Exit
          end
      | Leave(x) ->
          begin
            match node with
            | Leave(a) ->
                do_match_quoted a x acc eq_mode
            | _ ->
                raise Exit
          end
      | Force(x) ->
          begin
            match node with
            | Force(a) ->
                do_match_quoted a x acc eq_mode
            | _ ->
                raise Exit
          end
      | Lambda(body, frame, _, _, _) ->
          begin
            match node with
            | Lambda(body2, frame2, _, _, _) when frame = frame2 ->
                do_match_quoted body2 body acc eq_mode
            | _ ->
                raise Exit
          end
      | Integer(x) ->
          begin
            match node with
            | Integer(y) ->
                if Big_int.eq_big_int x y then
                  acc
                else
                  raise Exit
            | _ -> raise Exit
          end
      | Sym(x) ->
          begin
            match node with
            | Sym(y) -> if Symbol.eq x y then acc else raise Exit
            | _ -> raise Exit
          end
      | Nil | True | False ->
          if node == pat then
            acc
          else
            raise Exit
      | String(_) | Record(_) ->
          if not (is_const node) && node = pat then
            acc
          else
            raise Exit
      | Placeholder ->
          if eq_mode then
            begin
              if node == pat then acc else raise Exit
            end
          else
            node :: acc
      | Ignore ->
          if eq_mode then
            begin
              if node == pat then acc else raise Exit
            end
          else
            acc
      | Cons(x, y) ->
          begin
            match node with
            | Cons(a, b) -> do_match_quoted b y (do_match_quoted a x acc eq_mode) eq_mode
            | _ -> raise Exit
          end
      | Quoted(x) ->
          begin
            match node with
            | Quoted(a) ->
                do_match_quoted a x acc eq_mode
            | _ ->
                raise Exit
          end
      | _ ->
          failwith "bad pattern"
  end

let rec do_match node pat acc =
  if is_smallint pat then
    begin
      if pat == node then
        acc
      else
        raise Exit
    end
  else
    begin
      match pat with
      | Integer(x) ->
          begin
            match node with
            | Integer(y) ->
                if Big_int.eq_big_int x y then
                  acc
                else
                  raise Exit
            | _ -> raise Exit
          end
      | Sym(x) ->
          begin
            match node with
            | Sym(y) -> if Symbol.eq x y then acc else raise Exit
            | _ -> raise Exit
          end
      | Nil | True | False ->
          if node == pat then
            acc
          else
            raise Exit
      | String(_) | Record(_) ->
          if not (is_const node) && node = pat then
            acc
          else
            raise Exit
      | Proxy(rx) ->
          do_match node !rx acc
      | Placeholder ->
          node :: acc
      | Ignore ->
          acc
      | Cons(x, y) ->
          begin
            match node with
            | Cons(a, b) -> do_match b y (do_match a x acc)
            | _ -> raise Exit
          end
      | Quoted(x) ->
          begin
            match node with
            | Quoted(a) ->
                do_match_quoted a x acc false
            | _ ->
                raise Exit
          end
      | _ ->
          failwith "bad pattern"
    end

let pop_to env env_len frm =
  if frm = 0 then
    Env.empty
  else
    let n = env_len - frm
    in
    if n > 0 then
      match env with
      | _ :: t -> assert (env != Env.empty); Env.pop_n t (n - 1)
      | [] -> assert (env <> []); []
    else
      env

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
     BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
     Closure(_)
     (* is is very rare that we get a closure here -- only when sth
        (e.g. application) was not fully evaluated *)
     ->
       Delayed(ref (Closure(x, env, env_len)))
   | Lambda(body, frame, call_type, times_entered, attrs) ->
       LambdaClosure(body, Env.pop_n env (env_len - frame), frame, call_type, times_entered, attrs)
   | Var(n) -> assert (n < env_len); assert (env <> []);
       do_delay (Env.nth env n) [] 0 (* passing [] is OK since values in envs are closed, so the env won't be needed *)
   | _ -> x

let rec do_eval node env env_len =
  match node with
  | Appl(x, y, attrs) ->
      begin
        let x = do_eval x env env_len
        in
        match x with
        | Lambda(body, frame, CallByValue, _, _) ->
            let arg =
              match y with
              | Var(n) ->
                  begin
                    let y = Env.nth env n
                    in
                    if is_immediate y then y else do_eval y env env_len
                  end
              | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) | MakeRecord(_) | Closure(_) |
                BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
                BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
                Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
                Cons(_) | Quoted(_) | LambdaClosure(_) ->
                  do_eval y env env_len
              | _ ->
                  y
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
                      do_delay y env env_len
                    else
                      do_close y env env_len
                  and env2 = pop_to env env_len frame
                  and env2_len = frame
                  in
                  do_eval body (arg :: env2) (env2_len + 1)
            end

        | LambdaClosure(body, env2, env2_len, CallByValue, _, _) ->
            let arg =
              match y with
              | Var(n) ->
                  begin
                    let y = Env.nth env n
                    in
                    if is_immediate y then y else do_eval y env env_len
                  end
              | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
                MakeRecord(_) | Closure(_) |
                BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
                BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
                Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
                Cons(_) | Quoted(_) | LambdaClosure(_) ->
                  do_eval y env env_len
              | _ ->
                  y
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
                      do_delay y env env_len
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
      begin
        assert (n < env_len);
        let x = Env.nth env n
        in
        if is_const x then x else do_eval x env env_len
      end

  | Delayed(rx) ->
      begin
        let x = !rx
        in
        if is_immediate x then
          x
        else
          begin
            rx := do_eval x env env_len;
            !rx
          end
      end

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
      let node1 =
        match x with
        | Var(n) ->
            begin
              let x = Env.nth env n
              in
              if is_const x then x else do_eval x env env_len
            end
        | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
          MakeRecord(_) | Closure(_) |
          BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
          BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
          Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
          Cons(_) | Quoted(_) | LambdaClosure(_) ->
            do_eval x env env_len
        | _ ->
            x
      in
      let node2 =
        match y with
        | Var(n) ->
            begin
              let y = Env.nth env n
              in
              if is_const y then y else do_eval y env env_len
            end
        | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
          MakeRecord(_) | Closure(_) |
          BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
          BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
          Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
          Cons(_) | Quoted(_) | LambdaClosure(_) ->
            do_eval y env env_len
        | _ ->
            y
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
            List.hd
              (try
                do_match_quoted node1 node2 [True] true
              with Exit ->
                [False])
          with Unknown ->
            BEq(node1, node2)
        end

  | BGt(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                if is_const x then x else do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                if is_const y then y else do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Bignum.gt x y
      end

  | BGe(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                if is_const x then x else do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                if is_const y then y else do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Bignum.ge x y
      end

  | BAdd(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                if is_const x then x else do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                if is_const y then y else do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Bignum.add x y
      end

  | BSub(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                if is_const x then x else do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                if is_const y then y else do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Bignum.sub x y
      end

  | BMul(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                if is_const x then x else do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                if is_const y then y else do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Bignum.mul x y
      end

  | BIDiv(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                if is_const x then x else do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                if is_const y then y else do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Bignum.idiv x y
      end

  | BMod(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                if is_const x then x else do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                if is_const y then y else do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Bignum.modulo x y
      end

  | BCons(x, y) ->
      begin
        let x =
          match x with
          | Var(n) ->
              begin
                let x = Env.nth env n
                in
                do_eval x env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval x env env_len
          | _ ->
              x
        in
        let y =
          match y with
          | Var(n) ->
              begin
                let y = Env.nth env n
                in
                do_eval y env env_len
              end
          | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
            MakeRecord(_) | Closure(_) |
            BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
            BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
            Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
            Cons(_) | Quoted(_) | LambdaClosure(_) ->
              do_eval y env env_len
          | _ ->
              y
        in
        Cons(x, y)
      end

  | BConsNE(x, y) -> Cons(do_close x env env_len, do_close y env env_len)

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
        | (y, body, args_num) :: t ->
            begin
              let pat =
                match y with
                | Var(n) ->
                    begin
                      let y = Env.nth env n
                      in
                      if is_const y then y else do_eval y env env_len
                    end
                | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Delayed(_) | Proxy(_) |
                  MakeRecord(_) | Closure(_) |
                  BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
                  BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
                  Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
                  Cons(_) | Quoted(_) | LambdaClosure(_) ->
                    do_eval y env env_len
                | _ ->
                    y
              in
              match pat with
              | Sym(psym) ->
                  begin
                    match node with
                    | Sym(nsym) when Symbol.eq psym nsym ->
                        assert (args_num = 0);
                        do_eval body env env_len
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
                              do_eval body (nt :: nh :: env) (env_len + 2)
                          | Placeholder, Ignore ->
                              do_eval body (nh :: env) (env_len + 1)
                          | Ignore, Placeholder ->
                              do_eval body (nt :: env) (env_len + 1)
                          | Ignore, Ignore ->
                              do_eval body env env_len
                          | Placeholder, _ ->
                              begin
                                let env2 =
                                  try
                                    do_match nt pt (nh :: env)
                                  with Exit ->
                                    []
                                in
                                if env2 != [] then
                                  do_eval body env2 (env_len + args_num)
                                else
                                  loop node env env_len t
                              end
                          | Ignore, _ ->
                              begin
                                let env2 =
                                  try
                                    do_match nt pt env
                                  with Exit ->
                                    []
                                in
                                if env2 != [] then
                                  do_eval body env2 (env_len + args_num)
                                else
                                  loop node env env_len t
                              end
                          | _, Placeholder ->
                              begin
                                let env2 =
                                  try
                                    nt :: (do_match nh ph env)
                                  with Exit ->
                                    []
                                in
                                if env2 != [] then
                                  do_eval body env2 (env_len + args_num)
                                else
                                  loop node env env_len t
                              end
                          | _, Ignore ->
                              begin
                                let env2 =
                                  try
                                    do_match nh ph env
                                  with Exit ->
                                    []
                                in
                                if env2 != [] then
                                  do_eval body env2 (env_len + args_num)
                                else
                                  loop node env env_len t
                              end
                          | _ ->
                              begin
                                let env2 =
                                  try
                                    do_match nt pt (do_match nh ph env)
                                  with Exit ->
                                    []
                                in
                                if env2 != [] then
                                  do_eval body env2 (env_len + args_num)
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
                Record(_) | Quoted(_) | LambdaClosure(_)
                ->
                  begin
                    let env2 =
                      try
                        do_match node pat env
                      with Exit ->
                        []
                    in
                    if env2 != [] then
                      do_eval body env2 (env_len + args_num)
                    else
                      loop node env env_len t
                  end
              | _ ->
                  begin
                    if pat == Ignore then
                      begin
                        assert (args_num = 0);
                        do_eval body env env_len
                      end
                    else if pat == Placeholder then
                      begin
                        assert (args_num = 1);
                        do_eval body (node :: env) (env_len + 1)
                      end
                    else if pat == node then
                      begin
                        assert (args_num = 0);
                        do_eval body env env_len
                      end
                    else
                      loop node env env_len t
                  end
            end
        | [] -> failwith "match failure"
      in
      loop (do_eval x env env_len) env env_len branches

  | BRecordGet(x, y) ->
      begin
        let x = do_eval x env env_len in
        let y = do_eval y env env_len in
        match x, y with
        | Record(r), Sym(s) -> Symbol.Map.find s r
        | _ -> failwith "record error"
      end

  | _ -> node

let reduce node = do_eval node Env.empty 0

let eval node = do_eval node Env.empty 0

let eval_limited node limit = do_eval node Env.empty 0

let eval_in node env = do_eval node env (Env.length env)
