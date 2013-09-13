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

let max_lambda_body_frame_ref node frame0 =
  let rec aux node gap cframe acc =
    Traversal.traverse0
      (fun x m ->
        match x with
        | Node.Var(n) ->
            if n >= gap then
              Traversal.Continue(max (cframe - n) m)
            else
              Traversal.Continue(m)
        | Node.Lambda(body, frame, _, _, _) ->
            if frame < frame0 then
              Traversal.Skip(max (frame - 1) m)
            else
              Traversal.Skip(aux body (gap + 1) (cframe + 1) m)
        | Node.BMatch(x, branches) ->
            let rec aux2 lst acc =
              match lst with
              | (x, y, z, args_num) :: t ->
                  aux2 t (aux z (gap + args_num) (cframe + args_num)
                            (aux y (gap + args_num) (cframe + args_num)
                               (aux x gap cframe acc)))
              | [] ->
                  acc
            in
            Traversal.Skip(aux2 branches (aux x gap cframe m))
        | Node.LambdaClosure(_) | Node.Closure(_) ->
            Traversal.Skip(m)
        | _ -> Traversal.Continue(m)
      )
      node acc
  in
  aux node 1 frame0 (-1)

let correct_lambda node =
  let rec aux body frame frame2 call_type attrs gap frame0 =
    let shift = frame - frame2
    in
    let body2 =
      let rec aux2 node gap =
        Traversal.transform0
          (fun x ->
            match x with
            | Node.Lambda(body, frame3, call_type, _, attrs) ->
                if frame3 >= frame0 then
                  Traversal.Skip(aux body frame3 (frame3 - shift) call_type attrs (gap + 1) frame0)
                else
                  Traversal.Skip(x)
            | Node.BMatch(x, branches) ->
                let rec aux3 lst acc =
                  match lst with
                  | (x, y, z, n) :: t ->
                      aux3 t ((aux2 x gap, aux2 y (gap + n), aux2 z (gap + n), n) :: acc)
                  | [] ->
                      acc
                in
                Traversal.Skip(Node.BMatch(aux2 x gap, List.rev (aux3 branches [])))
            | _ ->
                Traversal.Continue(x))
          (fun x ->
            match x with
            | Node.Var(n) ->
                if n >= gap then
                  Node.Var(n - shift)
                else
                  Node.Var(n)
            | _ -> x)
          node
      in
      aux2 body gap
    in
    Node.Lambda(body2, frame2, call_type, ref 0, attrs)
  in
  match node with
  | Node.Lambda(body, frame, call_type, _, attrs) ->
      let frame2 = max_lambda_body_frame_ref body frame + 1
      in
      if frame2 <> frame then
        begin
          aux body frame frame2 call_type attrs 1 frame
        end
      else
        node
  | _ -> node

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
    Traversal.traverse0
      (fun x set ->
        match x with
        | Var(n) ->
            if cframe - n < frame0 then
              Traversal.Continue(IntSet.add (cframe - n) set)
            else
              Traversal.Continue(set)
        | Lambda(body, frame, _, _, _) ->
            if frame = 0 then
              Traversal.Skip(set)
            else
              Traversal.Skip(get_free_vars body (min frame0 frame) frame set)
        | BMatch(x, branches) ->
            let rec aux lst set =
              match lst with
              | (x, y, z, n) :: t ->
                  aux t (get_free_vars x frame0 cframe
                           (get_free_vars y frame0 (cframe + n)
                              (get_free_vars z frame0 (cframe + n) set)))
              | [] -> Traversal.Skip(set)
            in
            aux branches (get_free_vars x frame0 cframe set)
        | Closure(_) | LambdaClosure(_) ->
            Traversal.Skip(set)
        | _ ->
            Traversal.Continue(set)
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
  if Node.is_quoted node1 && Node.is_quoted node2 then
    begin
      let a = Node.unquote node1
      in
      try
        Traversal.traverse
          (fun x _ ->
            try
              if Match.equal_quoted x a then
                raise Exit
              else
                Traversal.Continue(false)
            with
              Match.Unknown -> Traversal.Continue(false)
          ) node2 false
      with Exit ->
        true
    end
  else
    Error.runtime_error "arguments of occurs-check should be quoted"

let subst node node1 node2 =
  if Node.is_quoted node && Node.is_quoted node1 && Node.is_quoted node2 then
    let node1 = Node.unquote node1
    and node2 = Node.unquote node2
    in
    Traversal.transform
      (fun x _ -> Traversal.Continue(x))
      (fun x ->
        if Match.equal_quoted x node1 then
          node2
        else
          x
      )
      node
  else
    Error.runtime_error "arguments of subst should be quoted"

let lift node node1 =
  if Node.is_quoted node && Node.is_quoted node1 then
    let unode1 = Node.unquote node1
    in
    let rec aux node =
      Traversal.transform
        (fun x frames_num ->
          if Match.equal_quoted x unode1 then
            begin
              Traversal.Skip(Var(frames_num))
            end
          else
            Traversal.Continue(x))
        (fun x ->
          match x with
          | Lambda(body, frame, call_type, times_entered, attrs) ->
              correct_lambda (Lambda(body, frame + 1, call_type, times_entered, attrs))
          | Quoted(y) -> y
          | _ -> x)
        node
    in
    let node2 = aux node
    in
    Quoted(Appl(Lambda(node2, 0, CallByValue, ref 0, None), unode1, None))
  else
    Error.runtime_error "arguments of lift should be quoted"
