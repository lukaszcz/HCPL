(* quote.ml: Operations on quoted nodes implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

let max_lambda_body_frame_ref node frame0 =
  let rec aux node cframe acc =
    assert (cframe >= frame0);
    Traversal.traverse0
      (fun x m ->
        match x with
        | Node.Var(n) ->
            if cframe - n < frame0 then
              Traversal.Continue(max (cframe - n) m)
            else
              Traversal.Continue(m)
        | Node.FrameRef(frm) ->
            if frm < frame0 then
              Traversal.Continue(max frm m)
            else
              Traversal.Continue(m)
        | Node.Lambda(body, frame, _, _, _) ->
            if frame < frame0 then
              Traversal.Skip(max (frame - 1) m)
            else
              Traversal.Skip(aux body frame m)
        | Node.BMatch(x, branches) ->
            let rec aux2 lst acc =
              match lst with
              | (x, y, z, args_num) :: t ->
                  aux2 t (aux z (cframe + args_num)
                            (aux y (cframe + args_num)
                               (aux x cframe acc)))
              | [] ->
                  acc
            in
            Traversal.Skip(aux2 branches (aux x cframe m))
        | Node.LambdaClosure(_) | Node.Closure(_) ->
            Traversal.Skip(m)
        | _ -> Traversal.Continue(m)
      )
      node acc
  in
  aux node frame0 (-1)

let correct_lambda node =
  (* shift_frames shifts frame0 to frame0 - shift *)
  let shift_frames node shift frame0 =
    let rec aux node cframe =
      Traversal.transform0
        (fun x ->
          match x with
          | Node.Lambda(body, frame3, call_type, _, attrs) ->
              if frame3 >= frame0 then
                Traversal.Skip(Node.Lambda(aux body frame3, frame3 - shift, call_type, ref 0, attrs))
              else
                Traversal.Skip(x)
          | Node.BMatch(x, branches) ->
              let rec aux2 lst acc =
                match lst with
                | (x, y, z, n) :: t ->
                    aux2 t ((aux x cframe, aux y (cframe + n), aux z (cframe + n), n) :: acc)
                | [] ->
                    acc
              in
              Traversal.Skip(Node.BMatch(aux x cframe, List.rev (aux2 branches [])))
          | _ ->
              Traversal.Continue(x))
        (fun x ->
          match x with
          | Node.Var(n) ->
              if cframe - n < frame0 then
                Node.Var(n - shift)
              else
                Node.Var(n)
          | _ -> x)
        node
    in
    aux node frame0
  in
  match node with
  | Node.Lambda(body, frame, call_type, _, attrs) ->
      let frame2 = max_lambda_body_frame_ref body frame + 1
      in
(*      Debug.print "correct_lambda";
      Debug.print (Node.to_string node);
      Debug.print_int frame2; *)
      if frame2 <> frame then
        begin
          assert (frame2 < frame);
          Node.Lambda(shift_frames body (frame - frame2) frame, frame2, call_type, ref 0, attrs)
        end
      else
        node
  | _ -> node

let rec do_close node env env_len is_node_quoted is_env_quoted =
  Traversal.transform
    (fun node env env_len _ ->
      match node with
      | Var(n) ->
          if n < env_len then
            begin
              let x = Env.nth env n
              in
              if x = Dummy then
                Traversal.Continue(node)
              else if is_env_quoted then
                Traversal.Continue(x)
              else
                let rec fix_fetched x =
                  match x with
                  | Quoted(y) ->
                        do_close y env env_len true is_env_quoted
                  | Integer(_) | String(_) | Record(_) | Sym(_) ->
                      x
                  | Cons(a, b) ->
                      Cons(fix_fetched a, fix_fetched b)
                  | _ ->
                      (* NOTE: the following check is necessary to ensure consistency of the logic *)
                      if Config.is_unsafe_mode () then
                        do_close x env env_len is_node_quoted is_env_quoted
                      else if is_const x then
                        x
                      else
                        begin
                          Error.runtime_error "cannot quote a non-constant value"
                        end
                in
                Traversal.Skip(fix_fetched x)
            end
          else
            Traversal.Continue(node)
      | Lambda(_, 0, _, _, _) -> Traversal.Skip(node)
      | Closure(_) | LambdaClosure(_) ->
          if is_env_quoted || not is_node_quoted then
            Traversal.Continue(node)
          else
            Traversal.Skip(do_close node env env_len true true)
      | Quoted(x) ->
          if is_node_quoted then
            Traversal.Continue(x)
          else
            Traversal.Skip(do_close x env env_len true is_env_quoted)
      | _ ->
          if Node.is_immed node then
            Traversal.Skip(node)
          else
            Traversal.Continue(node))
    (fun x -> assert (match x with Quoted(_) -> false | _ -> true); x)
    (if env_len = 0 then node else Closure(node, env, env_len))

let close node =
  if Node.is_quoted node then
    Node.mkquoted (do_close node [] 0 true true)
  else
    Error.runtime_error "the argument of 'close' should be quoted"

let get_free_vars node =
  let rec aux node frame0 cframe set =
    Traversal.traverse0
      (fun x set ->
        match x with
        | Var(n) ->
            if cframe - n < frame0 then
              Traversal.Continue(Utils.IntSet.add (cframe - n) set)
            else
              Traversal.Continue(set)
        | Lambda(body, frame, _, _, _) ->
            if frame = 0 then
              Traversal.Skip(set)
            else
              Traversal.Skip(aux body (min frame0 frame) frame set)
        | BMatch(x, branches) ->
            let rec aux2 lst set =
              match lst with
              | (x, y, z, n) :: t ->
                  aux2 t (aux x frame0 cframe
                            (aux y frame0 (cframe + n)
                               (aux z frame0 (cframe + n) set)))
              | [] -> Traversal.Skip(set)
            in
            aux2 branches (aux x frame0 cframe set)
        | Closure(_) | LambdaClosure(_) ->
            Traversal.Skip(set)
        | _ ->
            Traversal.Continue(set)
      )
      node set
  in
  aux node 0 (-1) Utils.IntSet.empty

let largest_frame node =
  let fvars = get_free_vars node
  in
  Utils.IntSet.fold (fun x acc -> max x acc) fvars (-1)

let eta_reduce node =
  if Node.is_quoted node then
    match Node.unquote node with
    | Lambda(Appl(x, Var(frame1), _), frame2, _, _, _)
      when
        frame1 = frame2 &&
        not (Utils.IntSet.mem frame1 (get_free_vars x)) ->
          Node.mkquoted (do_close (Closure(x, [], 0)) [] 0 true true)
    | _ -> node
  else
    Error.runtime_error "'reduce-eta' expects a quoted argument"

let quote node env env_len =
  if Node.is_quoted node then
    node
  else if env_len = 0 then
    Node.mkquoted node
  else
    begin
      let node2 = do_close node env env_len false false
      in
      assert (Utils.IntSet.is_empty (get_free_vars node2));
      Node.mkquoted node2
    end

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
      (fun x _ _ _ -> Traversal.Continue(x))
      (fun x ->
        if Match.equal_quoted x node1 then
          node2
        else
          x
      )
      node
  else
    Error.runtime_error "arguments of subst should be quoted"

let do_lift node f =
  let rec aux node =
    Traversal.transform
      (fun x _ _ frames_num ->
        if f x then
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
  assert (Utils.IntSet.is_empty (get_free_vars (Lambda(node2, 0, CallByValue, ref 0, None))));
  Lambda(node2, 0, CallByValue, ref 0, None)

let lift node node1 =
  if Node.is_quoted node && Node.is_quoted node1 then
    let unode1 = Node.unquote node1
    in
    assert (Utils.IntSet.is_empty (get_free_vars unode1));
    let node2 = do_lift node (fun x -> Match.equal_quoted x unode1)
    in
    Quoted(Appl(node2, unode1, None))
  else
    Error.runtime_error "arguments of lift should be quoted"

let lift_marked node node1 =
  if Node.is_quoted node && Node.is_quoted node1 then
    let unode1 = Node.unquote node1
    in
    assert (Utils.IntSet.is_empty (get_free_vars unode1));
    Quoted(do_lift
             node
             (fun x ->
               match x with
               | Marked(_, m) -> Match.equal_quoted m unode1
               | _ -> false))
  else
    Error.runtime_error "arguments of lift-marked should be quoted"
