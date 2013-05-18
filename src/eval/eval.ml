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

let close x env env_len =
  match x with
  | Builtin(_) | Integer(_) | True | False | Cons(_) | Nil | Data(_) |
    Closure(_)
    -> x
  | Lambda(_, frame, _, _) -> Closure(x, pop_n env (env_len - frame), frame)
  | LambdaEager(_, frame, _, _) -> Closure(x, pop_n env (env_len - frame), frame)
  | _ -> Closure(x, env, env_len)

let eval_limited node limit =

  (* The evaluator is essentially a variant of the ZINC machine. *)

  (* Invariant: Values in environments are closed. Values on the stack
     need not be. *)

  (* TODO: stack should be changed to a mutable stack to avoid
     unnecessary list allocation *)

  (* In the shift state the arguments are shifted onto the stack. *)
  let rec shift node stack env env_len =
    (* env is the environment both for node and for the values on the stack *)
    match node with
    | Progn(lst, _) ->
        begin
          match lst with
          | h :: t -> shift h (ReturnProgn(t) :: stack) env env_len
          | [] -> return Node.Nil stack env env_len env env_len
        end

    | Appl(x, y, _) ->
        shift x (y :: stack) env env_len

    | Cond(x, y, z, _) ->
        shift x ((ReturnCond(y, z)) :: stack) env env_len

    | Var(n) ->
        shift (nth env n) stack env env_len

    | Fail(x, attrs) ->
        reduce (Error(close x env env_len, attrs)) stack env env_len env env_len

    | Delayed(rx) ->
        if is_immediate !rx then
          return !rx stack env env_len env env_len
        else
          shift !rx ((Store(rx)) :: stack) env env_len

    | Proxy(rx) ->
        shift !rx stack env env_len

    | Lambda(_) | LambdaEager(_) | Builtin(_) | Error(_) ->
        reduce node stack env env_len env env_len

    | Closure(x, env2, env2_len) ->
        reduce x stack env2 env2_len env env_len

    | Force(x) ->
        shift x stack env env_len

    | Delay(x) ->
        return (Delayed(ref x)) stack env env_len env env_len

    | Integer(_) | True | False | Cons(_, _) | Nil | Data(_, _) ->
        return node stack env env_len env env_len

    | _ -> assert false

  (* In the reduce state the stack is examined and its contents is
     evaluated and/or placed in the environment. *)
  and reduce node stack env env_len s_env s_env_len =
    (* env is the environment for node and s_env is the environment
       for the values on the stack *)
    match node with
    | Lambda(body, frame, num_entered, _) ->
        if limit = -1 then
          begin
            match stack with
            | ChangeStackEnv(env2, env2_len) :: t ->
                reduce node t env env_len env2 env2_len
            | Store(rx) :: t ->
                begin
                  rx := close node env env_len;
                  reduce node t env env_len s_env s_env_len
                end
            | ReturnProgn(_) :: _ | ReturnApply(_) :: _ | ReturnCond(_) :: _ ->
                return node stack env env_len s_env s_env_len
            | (Force(x)) :: t ->
                begin
                  assert (env_len >= frame);
                  let env2 = pop_n env (env_len - frame)
                  and env2_len = frame
                  in
                  match x with
                  | Var(n) ->
                      let arg = nth s_env n
                      in
                      if is_immediate arg then
                        reduce body t (arg :: env2) (env2_len + 1) s_env s_env_len
                      else
                        shift arg (ReturnApply(body, env2, env2_len) :: t) s_env s_env_len
                  | Integer(_) | True | False | Cons(_, _) | Nil | Data(_, _) ->
                      reduce body t (x :: env2) (env2_len + 1) s_env s_env_len
                  | _ ->
                      shift x ((ReturnApply(body, env2, env2_len)) :: t) s_env s_env_len
                end
            | h :: t ->
                assert (env_len >= frame);
                let env2 = pop_n env (env_len - frame)
                and env2_len = frame
                in
                reduce body t (Delayed(ref (close h s_env s_env_len)) :: env2) (env2_len + 1) s_env s_env_len
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

    | LambdaEager(body, frame, num_entered, _) ->
        if limit = -1 then
          begin
            match stack with
            | ChangeStackEnv(env2, env2_len) :: t ->
                reduce node t env env_len env2 env2_len
            | Store(rx) :: t ->
                begin
                  rx := close node env env_len;
                  reduce node t env env_len s_env s_env_len
                end
            | ReturnProgn(_) :: _ | ReturnApply(_) :: _ | ReturnCond(_) :: _ ->
                return node stack env env_len s_env s_env_len
            | h :: t ->
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
                  | Integer(_) | True | False | Cons(_) | Nil | Data(_) ->
                      reduce body t (h :: env2) (env2_len + 1) s_env s_env_len
                  | _ ->
                      shift h ((ReturnApply(body, env2, env2_len)) :: t) s_env s_env_len
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

    | Progn(lst, _) ->
        begin
          match lst with
          | h :: t -> shift h (ReturnProgn(t) :: ChangeStackEnv(s_env, s_env_len) :: stack) env env_len
          | [] -> return Node.Nil stack env env_len s_env s_env_len
        end

    | Appl(x, y, _) ->
        shift x (y :: (ChangeStackEnv(s_env, s_env_len)) :: stack) env env_len

    | Var(n) ->
        reduce (nth env n) stack env env_len s_env s_env_len

    | Proxy(rx) ->
        reduce !rx stack env env_len s_env s_env_len

    | Integer(_) | True | False | Cons(_, _) | Nil | Data(_, _) ->
        return node stack env env_len s_env s_env_len

    | Error(x, attrs) ->
        failwith "Errors not yet implemented"

    | _ -> shift node ((ChangeStackEnv(s_env, s_env_len)) :: stack) env env_len

  and return node stack env env_len s_env s_env_len =
    match stack with
    | ChangeStackEnv(env2, env2_len) :: t ->
        return node t env env_len env2 env2_len
    | Store(rx) :: t ->
        begin
          rx := close node env env_len;
          return node t env env_len s_env s_env_len
        end
    | ReturnProgn(lst) :: t ->
        begin
          match lst with
          | h :: t2 -> shift h (ReturnProgn(t2) :: t) s_env s_env_len
          | [] -> return node t env env_len s_env s_env_len
        end
    | ReturnApply(body2, env2, env2_len) :: t ->
        reduce body2 t ((close node env env_len) :: env2) (env2_len + 1) s_env s_env_len
    | ReturnCond(x, y) :: t ->
        begin
          match node with
          | True -> shift x t s_env s_env_len
          | False -> shift y t s_env s_env_len
          | _ ->
              return
                (Cond(node, close x s_env s_env_len, close y s_env s_env_len, None))
                t env env_len s_env s_env_len
        end
    | h :: t ->
        return (Appl(node, close h s_env s_env_len, None)) t env env_len s_env s_env_len
    | [] ->
        if env = [] then
          node
        else
          close node env env_len
  in
  shift node [] [] 0

let reduce node = eval_limited node 1

let eval node = eval_limited node (-1)
