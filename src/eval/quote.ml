(* quote.ml: Operations on quoted nodes implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

module OrderedType =
  struct
    type t = int
    let compare = compare
  end

module IntSet = Set.Make(OrderedType)

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
  let rec get_free_vars node frame0 cframe set =
    NodeUtils.traverse
      (fun x set ->
        match x with
        | Var(n) ->
            if cframe - n < frame0 then
              NodeUtils.Continue(IntSet.add (cframe - n) set)
            else
              NodeUtils.Continue(set)
        | Lambda(body, frame, _, _, _) ->
            if frame = 0 then
              NodeUtils.Skip(set)
            else
              NodeUtils.Skip(get_free_vars body (min frame0 frame) frame set)
        | BMatch(x, branches) ->
            let rec aux lst set =
              match lst with
              | (x, y, z, n) :: t ->
                  aux t (get_free_vars x frame0 cframe
                           (get_free_vars y frame0 (cframe + n)
                              (get_free_vars z frame0 (cframe + n) set)))
              | [] -> NodeUtils.Skip(set)
            in
            aux branches (get_free_vars x frame0 cframe set)
        | Closure(_) | LambdaClosure(_) ->
            NodeUtils.Skip(set)
        | _ ->
            NodeUtils.Continue(set)
      )
      node set
  in
  let rec filter lst set n acc =
    match lst with
    | h :: t ->
        if IntSet.mem n set then
          filter t set (n + 1) (h :: acc)
        else
          filter t set (n + 1) (Nil :: acc)
    | [] -> acc
  in
  let env_len = Env.length env
  in
  if env_len = 0 || Node.is_closed node then
    Node.mkquoted node
  else
    let set = get_free_vars node env_len (env_len - 1) IntSet.empty
    in
    if IntSet.is_empty set then
      Node.mkquoted node
    else
      let env2 = filter (List.rev env) set 0 []
      in
      Node.mkquoted (Closure(node, List.map fix_node_in_env env2, env_len))

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
