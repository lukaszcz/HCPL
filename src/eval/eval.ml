(* eval.ml: Evaluator implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

let rec nth lst n =
  assert (n >= 0);
  match lst with
  | h :: t ->
      if n > 0 then
        nth t (n - 1)
      else
        h
  | [] -> assert false

let rec pop_n lst n =
  assert (n >= 0);
  if n > 0 then
    match lst with
    | h :: t -> pop_n t (n - 1)
    | [] -> assert false
  else
    lst

let do_close x env env_len =
  match x with
  | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) | True | False | Cons(_) | Nil |
    Closure(_)
    -> x
  | Lambda(_, frame, _, _, _) -> Closure(x, pop_n env (env_len - frame), frame)
  | _ -> Closure(x, env, env_len)

let change_stack_env stack env env_len =
  match stack with
  | ChangeStackEnv(_) :: _ -> stack
  | _ -> ChangeStackEnv(env, env_len) :: stack

let do_eval node limit env =

  (* The evaluator is essentially a variant of the ZINC machine. *)

  (* Invariant: Values in environments are closed. Values on the stack
     need not be. *)

  (* TODO: stack should be changed to a mutable stack to avoid
     unnecessary list allocation *)

  (* In the shift state the arguments are shifted onto the stack. *)
  let rec shift node stack env env_len =
    (* env is the environment both for node and for the values on the stack *)
    match node with
    | Appl(x, y, _) ->
        shift x (y :: stack) env env_len

    | Cond(x, y, z, _) ->
        shift x ((ReturnCond(y, z)) :: stack) env env_len

    | Var(n) ->
        shift (nth env n) stack env env_len

    | Delayed(rx) ->
        if is_immediate !rx then
          shift !rx stack env env_len
        else
          shift !rx ((Store(rx)) :: stack) env env_len

    | Proxy(rx) ->
        shift !rx stack env env_len

    | Lambda(_) | Builtin(_) ->
        reduce node stack env env_len env env_len

    | Closure(x, env2, env2_len) ->
        reduce x stack env2 env2_len env env_len

    | Force(x) ->
        shift x stack env env_len

    | Delay(x) ->
        return (Delayed(ref x)) stack env env_len env env_len

    | Leave(x) ->
        return x stack env env_len env env_len

    | MakeRecord(identtab) ->
        return (Record(Symbol.Map.map (fun x -> do_close x env env_len) identtab)) stack env env_len env env_len

    | Integer(_) | String(_) | Record(_) | Sym(_) | True | False | Cons(_, _) | Nil ->
        return node stack env env_len env env_len

    | _ -> assert false

  (* In the reduce state the stack is examined and its contents is
     evaluated and/or placed in the environment. *)
  and reduce node stack env env_len s_env s_env_len =
    (* env is the environment for node and s_env is the environment
       for the values on the stack *)
    match node with
    | Lambda(body, frame, call_type, num_entered, _) ->
        if limit = -1 then
          begin
            match stack with
            | ChangeStackEnv(env2, env2_len) :: t ->
                reduce node t env env_len env2 env2_len
            | Store(rx) :: t ->
                begin
                  rx := do_close node env env_len;
                  reduce node t env env_len s_env s_env_len
                end
            | ReturnApply(_) :: _ | ReturnCond(_) :: _ ->
                return node stack env env_len s_env s_env_len
            | h :: t ->
                begin
                  assert (env_len >= frame);
                  let env2 = pop_n env (env_len - frame)
                  and env2_len = frame
                  in
                  if call_type = CallByValue then
                    begin
                      assert (env_len >= frame);
                      let env2 = pop_n env (env_len - frame)
                      and env2_len = frame
                      in
                      match h with
                      | Var(n) ->
                          let arg = nth s_env n
                          in
                          if is_immediate arg then
                            reduce body t (arg :: env2) (env2_len + 1) s_env s_env_len
                          else
                            shift arg (ReturnApply(body, env2, env2_len) :: t) s_env s_env_len
                      | Integer(_) | String(_) | Record(_) | Sym(_) | True | False | Cons(_) | Nil ->
                          reduce body t (h :: env2) (env2_len + 1) s_env s_env_len
                      | _ ->
                          shift h ((ReturnApply(body, env2, env2_len)) :: t) s_env s_env_len
                    end
                  else
                    begin
                      match h with
                      | Force(x) ->
                          begin
                            match x with
                            | Var(n) ->
                                let arg = nth s_env n
                                in
                                if is_immediate arg then
                                  reduce body t (arg :: env2) (env2_len + 1) s_env s_env_len
                                else
                                  shift arg (ReturnApply(body, env2, env2_len) :: t) s_env s_env_len
                            | Integer(_) | String(_) | Record(_) | Sym(_) | True | False | Cons(_, _) | Nil ->
                                reduce body t (x :: env2) (env2_len + 1) s_env s_env_len
                            | _ ->
                                shift x ((ReturnApply(body, env2, env2_len)) :: t) s_env s_env_len
                          end
                      | Leave(x) ->
                          reduce body t ((do_close x s_env s_env_len) :: env2) (env2_len + 1) s_env s_env_len
                      | _ ->
                          let x =
                            if call_type = CallByNeed then
                              Delayed(ref (do_close h s_env s_env_len))
                            else
                              do_close h s_env s_env_len
                          in
                          reduce body t (x :: env2) (env2_len + 1) s_env s_env_len
                    end
                end
            | [] ->
                assert (env_len >= frame);
                let env2 = pop_n env (env_len - frame)
                in
                Closure(node, env2, frame)
          end
        else
          begin
            failwith "Not yet implemented"
          end

    | Builtin(func, args_num, _) ->
        assert (args_num >= env_len);
        if limit != 0 then
          reduce (func env) stack (pop_n env args_num) (env_len - args_num) s_env s_env_len
        else
          return node stack env env_len s_env s_env_len

    | Closure(x, env2, env2_len) ->
        reduce x stack env2 env2_len s_env s_env_len

    | Appl(x, y, _) ->
        shift x (y :: (change_stack_env stack s_env s_env_len)) env env_len

    | Var(n) ->
        reduce (nth env n) stack env env_len s_env s_env_len

    | Proxy(rx) ->
        reduce !rx stack env env_len s_env s_env_len

    | Integer(_) | String(_) | Record(_) | Sym(_) | True | False | Cons(_, _) | Nil ->
        return node stack env env_len s_env s_env_len

    | _ -> shift node (change_stack_env stack s_env s_env_len) env env_len

  and return node stack env env_len s_env s_env_len =
    match stack with
    | ChangeStackEnv(env2, env2_len) :: t ->
        return node t env env_len env2 env2_len
    | Store(rx) :: t ->
        begin
          rx := do_close node env env_len;
          return node t env env_len s_env s_env_len
        end
    | ReturnApply(body2, env2, env2_len) :: t ->
        reduce body2 t ((do_close node env env_len) :: env2) (env2_len + 1) s_env s_env_len
    | ReturnCond(x, y) :: t ->
        begin
          match node with
          | True -> shift x t s_env s_env_len
          | False -> shift y t s_env s_env_len
          | _ ->
              return
                (Cond(node, do_close x s_env s_env_len, do_close y s_env s_env_len, None))
                t env env_len s_env s_env_len
        end
    | h :: t ->
        return (Appl(node, do_close h s_env s_env_len, None)) t env env_len s_env s_env_len
    | [] ->
        if env = [] then
          node
        else
          do_close node env env_len
  in
  shift node [] env (List.length env)

let reduce node = do_eval node 1 []

let eval node = do_eval node (-1) []

let eval_limited node limit = do_eval node limit []

let eval_in node env = do_eval node (-1) env
