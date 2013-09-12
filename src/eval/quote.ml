(* quote.ml: Operations on quoted nodes implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

let do_close node env env_len =
  if env_len = 0 || Node.is_closed node then
    node
  else
    Closure(node, env, env_len)

let quote node env =
  let rec fix_node_in_env node =
    match node with
    | Quoted(x) -> x
    | Integer(_) | String(_) | Record(_) | Sym(_) ->
        node
    | Cons(a, b) ->
        if Config.is_unsafe_mode () then
          node
        else
          Cons(fix_node_in_env a, fix_node_in_env b)
    | _ ->
        (* NOTE: the following check is necessary to ensure consistency of the logic *)
        if Config.is_unsafe_mode () then
          node
        else if is_const node then
          node
        else
          begin
            Error.runtime_error "cannot quote a non-constant value"
          end
  in
  Node.mkquoted (do_close node (List.map fix_node_in_env env) (Env.length env))

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
      Error.runtime_error "expected a quoted value as the first argument of occurs-check"
