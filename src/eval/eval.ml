(* eval.ml: Evaluator implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

(*
   Invariants:
      - nodes stored in environments are closed
      - do_eval returns a closed node
      - quoted nodes (i.e. inside Quoted(_)) are closed
      - quoted nodes do not contain any Closure, LambdaClosure or
        Delayed nodes unless Config.is_unsafe_mode () returns true
*)

exception Unknown

type match_quoted_mode_t = ModeMatch | ModeEq | ModeQuotedEq

let rec do_match_quoted node pat acc (mode : match_quoted_mode_t) =
  let node = Node.normalize node in
  let pat = Node.normalize pat in
  begin
    if is_smallint pat then
      begin
        if node == pat then
          acc
        else
          raise Exit
      end
    else if pat == Placeholder then
      begin
        if mode <> ModeMatch then
          begin
            if node == pat then acc else raise Exit
          end
        else
          (Node.quote node) :: acc
      end
    else if pat == Ignore then
      begin
        if mode <> ModeMatch then
          begin
            if node == pat then acc else raise Exit
          end
        else
          acc
      end
    else if node == pat then
      acc
    else if mode = ModeEq && not (is_immediate node && is_immediate pat) then
      begin
        raise Unknown
      end
    else
      match pat with
      | Appl(x, y, _) ->
          begin
            match node with
            | Appl(a, b, _) -> do_match_quoted b y (do_match_quoted a x acc mode) mode
            | _ -> raise Exit
          end
      | Cond(x, y, z, _) ->
          begin
            match node with
            | Cond(a, b, c, _) ->
                do_match_quoted c z (do_match_quoted b y (do_match_quoted a x acc mode) mode) mode
            | _ ->
                raise Exit
          end
      | Delay(x) ->
          begin
            match node with
            | Delay(a) ->
                do_match_quoted a x acc mode
            | _ ->
                raise Exit
          end
      | Leave(x) ->
          begin
            match node with
            | Leave(a) ->
                do_match_quoted a x acc mode
            | _ ->
                raise Exit
          end
      | Force(x) ->
          begin
            match node with
            | Force(a) ->
                do_match_quoted a x acc mode
            | _ ->
                raise Exit
          end
      | Lambda(body, frame, _, _, _) ->
          begin
            match node with
            | Lambda(body2, frame2, _, _, _) when frame = frame2 ->
                do_match_quoted body2 body acc mode
            | _ ->
                raise Exit
          end
      | Var(n) ->
          begin
            match node with
            | Var(m) when m = n -> acc
            | _ -> raise Exit
          end
      | Integer(x) ->
          begin
            match node with
            | Integer(y) when Big_int.eq_big_int x y -> acc
            | _ -> raise Exit
          end
      | Sym(x) ->
          begin
            match node with
            | Sym(y) when Symbol.eq x y -> acc
            | _ -> raise Exit
          end
      | Nil | True | False ->
          if node == pat then
            acc
          else
            raise Exit
      | String(x) ->
          begin
            match node with
            | String(y) when x = y -> acc
            | _ -> raise Exit
          end
      | Record(x) ->
          begin
            match node with
            | Record(y) when x = y -> acc (* TODO: this is wrong *)
            | _ -> raise Exit
          end
      | Cons(x, y) ->
          begin
            match node with
            | Cons(a, b) -> do_match_quoted b y (do_match_quoted a x acc mode) mode
            | _ -> raise Exit
          end
      | Quoted(x) ->
          begin
            match node with
            | Quoted(a) -> do_match_quoted a x acc (if mode = ModeEq then ModeQuotedEq else mode)
            | _ -> raise Exit
          end
      | BEq(px, py) ->
          begin
            match node with
            | BEq(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BGt(px, py) ->
          begin
            match node with
            | BGt(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BGe(px, py) ->
          begin
            match node with
            | BGe(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BAdd(px, py) ->
          begin
            match node with
            | BAdd(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BSub(px, py) ->
          begin
            match node with
            | BSub(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BMul(px, py) ->
          begin
            match node with
            | BMul(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BIDiv(px, py) ->
          begin
            match node with
            | BIDiv(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BMod(px, py) ->
          begin
            match node with
            | BMod(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BCons(px, py) ->
          begin
            match node with
            | BCons(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BConsNE(px, py) ->
          begin
            match node with
            | BConsNE(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BFst(px) ->
          begin
            match node with
            | BFst(nx) -> do_match_quoted nx px acc mode
            | _ -> raise Exit
          end
      | BSnd(px) ->
          begin
            match node with
            | BSnd(nx) -> do_match_quoted nx px acc mode
            | _ -> raise Exit
          end
      | BNot(px) ->
          begin
            match node with
            | BNot(nx) -> do_match_quoted nx px acc mode
            | _ -> raise Exit
          end
      | BAnd(px, py) ->
          begin
            match node with
            | BAnd(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BOr(px, py) ->
          begin
            match node with
            | BOr(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | BMatch(px, plst) ->
          begin
            match node with
            | BMatch(nx, nlst) ->
                List.fold_left2
                  (fun a (n1, n2, _) (p1, p2, _) ->
                    do_match_quoted n2 p2 (do_match_quoted n1 p1 a mode) mode)
                  (do_match_quoted nx px acc mode) nlst plst
            | _ -> raise Exit
          end
      | BRecordGet(px, py) ->
          begin
            match node with
            | BRecordGet(nx, ny) -> do_match_quoted ny py (do_match_quoted nx px acc mode) mode
            | _ -> raise Exit
          end
      | Delayed(r) ->
          begin
            match node with
            | Delayed(r0) -> do_match_quoted !r0 !r acc mode
            | _ -> raise Exit
          end
      | _ ->
          failwith "bad pattern"
  end

let rec do_match node pat acc =
  match pat with
  | Sym(psym) ->
      begin
        match node with
        | Sym(nsym) when Symbol.eq psym nsym -> acc
        | _ -> raise Exit
      end
  | Cons(px, py) ->
      begin
        match node with
        | Cons(nx, ny) -> do_match ny py (do_match nx px acc)
        | _ -> raise Exit
      end
  | Integer(x) ->
      begin
        match node with
        | Integer(y) when Big_int.eq_big_int x y -> acc
        | _ -> raise Exit
      end
  | String(x) ->
      begin
        match node with
        | String(y) when x = y -> acc
        | _ -> raise Exit
      end
  | Record(x) ->
      begin
        match node with
        | Record(y) when x = y -> acc
        | _ -> raise Exit
      end
  | Quoted(x) ->
      begin
        match node with
        | Quoted(y) -> do_match_quoted y x acc ModeMatch
        | _ -> raise Exit
      end
  | Appl(_) | Cond(_) | Delay(_) | Leave(_) | Force(_) | Var(_) | Proxy(_) | MakeRecord(_) |
    BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
    BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) |
    Closure(_) | Delayed(_) | Lambda(_) | Builtin(_) | LambdaClosure(_)
    ->
      failwith "bad pattern"
  | _ ->
      begin
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
            | Placeholder ->
                node :: acc
            | Ignore ->
                acc
            | _ ->
                if node == pat then
                  acc
                else
                  raise Exit
          end
      end

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
    | _ -> assert (is_immed x || (match x with Lambda(_, 0, _, _, _) -> true | _ -> false)); x
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
      True | False | Placeholder | Ignore | Cons(_) | Nil | Quoted(_) |
      LambdaClosure(_)| Unboxed1 | Unboxed2 | Unboxed3 | Unboxed4 | Unboxed5
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
              let env2 = pop_to env env_len frame
              and env2_len = frame
              in
              do_eval body (arg :: env2) (env2_len + 1)
            else
              Appl(x, arg, attrs)

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
            List.hd
              (try
                do_match_quoted node1 node2 [True] ModeEq
              with Exit ->
                [False])
          with Unknown ->
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
              let pat = do_eval y env env_len
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
                                    dummy_env
                                in
                                if env2 != dummy_env then
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
                                    dummy_env
                                in
                                if env2 != dummy_env then
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
                                    dummy_env
                                in
                                if env2 != dummy_env then
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
                                    dummy_env
                                in
                                if env2 != dummy_env then
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
                                    dummy_env
                                in
                                if env2 != dummy_env then
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
                        dummy_env
                    in
                    if env2 != dummy_env then
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

let eval node = eval_limit := -1; do_eval node Env.empty 0

let eval_limited node limit =
  eval_limit := limit;
  let r = do_eval node Env.empty 0
  in
  Stack.iter (fun x -> x := 0) refstack;
  Stack.clear refstack;
  r

let reduce node = eval_limited node 1

let eval_in node env = eval_limit := -1; do_eval node env (Env.length env)
