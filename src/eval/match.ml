(* match.ml: Matching implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

exception Unknown

type match_quoted_mode_t = ModeMatch | ModeEq | ModeQuotedEq

let [@warning "-39"]rec check_tokens_eq lst1 lst2 =
  match lst1, lst2 with
  | ((tok1, _) :: _), ((tok2, _) :: _) when Token.eq tok1 tok2 -> true
  | _ -> false

let rec do_match_quoted node pat penv penv_len nenv nenv_len acc (mode : match_quoted_mode_t) =
  let do_match_indeed () =
    let node = Node.normalize node in
    match pat with
    | Proxy(r) ->
        begin
          match !r with
          | Dummy2(pat2) ->
              if node == pat2 then
                acc
              else
                raise Exit
          | pat2 ->
              r := Dummy2(node);
              Utils.try_finally
                (fun () -> do_match_quoted node pat2 penv penv_len nenv nenv_len acc mode)
                (fun () -> r := pat2)
        end
    | _ ->
        let pat = Node.normalize pat in
        begin
          if is_smallint pat then
            begin
              if node == pat then
                acc
              else if Bignum.is_number node || mode <> ModeEq then
                raise Exit
              else
                raise Unknown
            end
          else if pat == Placeholder then
            begin
              if mode <> ModeMatch then
                begin
                  (* TODO: This is wrong. *)
                  if node == pat then
                    acc
                  else
                    raise Exit
                end
              else
                let node2 =
                  if nenv_len = 0 || Node.is_closed node then
                    node
                  else if List.hd nenv = Dummy then
                    begin
                      if Node.is_closed node || Config.is_unsafe_mode () then
                        node
                      else
                        Error.runtime_error "cannot match a value possibly containing free variables"
                    end
                  else
                    Closure(node, nenv, nenv_len)
                in
                (Node.mkquoted node2) :: acc
            end
          else if pat == Ignore then
            begin
              if mode <> ModeMatch then
                begin
                  (* TODO: This is wrong. *)
                  if node == pat then acc else raise Exit
                end
              else
                acc
            end
          else if node == pat && (Node.is_closed node || (penv_len = 0 && nenv_len = 0)) then
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
                  | Appl(a, b, _) ->
                      do_match_quoted b y penv penv_len nenv nenv_len
                        (do_match_quoted a x penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | Cond(x, y, z, _) ->
                begin
                  match node with
                  | Cond(a, b, c, _) ->
                      do_match_quoted c z penv penv_len nenv nenv_len
                        (do_match_quoted b y penv penv_len nenv nenv_len
                           (do_match_quoted a x penv penv_len nenv nenv_len acc mode) mode) mode
                  | _ ->
                      raise Exit
                end
            | Delay(x) ->
                begin
                  match node with
                  | Delay(a) ->
                      do_match_quoted a x penv penv_len nenv nenv_len acc mode
                  | _ ->
                      raise Exit
                end
            | Leave(x) ->
                begin
                  match node with
                  | Leave(a) ->
                      do_match_quoted a x penv penv_len nenv nenv_len acc mode
                  | _ ->
                      raise Exit
                end
            | Force(x) ->
                begin
                  match node with
                  | Force(a) ->
                      do_match_quoted a x penv penv_len nenv nenv_len acc mode
                  | _ ->
                      raise Exit
                end
            | Lambda(body, frame, _, _, _) ->
                begin
                  match node with
                  | Lambda(body2, frame2, _, _, _) ->
                      if frame > penv_len || frame2 > nenv_len then
                        begin
                          raise Exit
                        end
                      else
                        begin
                          let penv2 = Env.pop_n penv (penv_len - frame)
                          and nenv2 = Env.pop_n nenv (nenv_len - frame2)
                          in
                          if frame <> frame2 &&
                            ((penv2 <> [] && List.hd penv2 = Dummy) ||
                            (nenv2 <> [] && List.hd nenv2 = Dummy)) then
                            begin
                              raise Exit
                            end
                          else
                            do_match_quoted body2 body
                              (Dummy :: penv2) (frame + 1)
                              (Dummy :: nenv2) (frame2 + 1) acc mode
                        end
                  | _ ->
                      raise Exit
                end
            | Var(n) ->
                if n >= penv_len then
                  raise Exit
                else
                  begin
                    let pat2 = Env.nth penv n
                    in
                    if pat2 = Dummy then
                      match node with
                      | Var(m) when m = n -> acc
                      | _ -> raise Exit
                    else
                      do_match_quoted node pat2 penv penv_len nenv nenv_len acc mode
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
                  | Cons(a, b) ->
                      do_match_quoted b y penv penv_len nenv nenv_len
                        (do_match_quoted a x penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | Tokens(x) ->
                begin
                  match node with
                  | Tokens(y) -> if check_tokens_eq x y then acc else raise Exit
                  | _ -> raise Exit
                end
            | Quoted(x) ->
                begin
                  match node with
                  | Quoted(a) ->
                      do_match_quoted a x penv penv_len nenv nenv_len acc (if mode = ModeEq then ModeQuotedEq else mode)
                  | _ -> raise Exit
                end
            | BEq(px, py) ->
                begin
                  match node with
                  | BEq(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BGt(px, py) ->
                begin
                  match node with
                  | BGt(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BGe(px, py) ->
                begin
                  match node with
                  | BGe(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BAdd(px, py) ->
                begin
                  match node with
                  | BAdd(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BSub(px, py) ->
                begin
                  match node with
                  | BSub(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BMul(px, py) ->
                begin
                  match node with
                  | BMul(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BIDiv(px, py) ->
                begin
                  match node with
                  | BIDiv(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BMod(px, py) ->
                begin
                  match node with
                  | BMod(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BCons(px, py) ->
                begin
                  match node with
                  | BCons(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BConsNE(px, py) ->
                begin
                  match node with
                  | BConsNE(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BFst(px) ->
                begin
                  match node with
                  | BFst(nx) ->
                      do_match_quoted nx px penv penv_len nenv nenv_len acc mode
                  | _ -> raise Exit
                end
            | BSnd(px) ->
                begin
                  match node with
                  | BSnd(nx) ->
                      do_match_quoted nx px penv penv_len nenv nenv_len acc mode
                  | _ -> raise Exit
                end
            | BAnd(px, py) ->
                begin
                  match node with
                  | BAnd(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BOr(px, py) ->
                begin
                  match node with
                  | BOr(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | BMatch(px, plst) ->
                begin
                  match node with
                  | BMatch(nx, nlst) ->
                      List.fold_left2
                        (fun a (n1, n2, n3, num) (p1, p2, p3, num2) ->
                          if num <> num2 then
                            raise Exit
                          else
                            let penv2 = Env.push_n penv Dummy num
                            and penv2_len = penv_len + num
                            and nenv2 = Env.push_n nenv Dummy num
                            and nenv2_len = nenv_len + num
                            in
                            do_match_quoted n3 p3 penv2 penv2_len nenv2 nenv2_len
                              (do_match_quoted n2 p2 penv2 penv2_len nenv2 nenv2_len
                                 (do_match_quoted n1 p1 penv penv_len nenv nenv_len a mode) mode) mode
                        )
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) nlst plst
                  | _ -> raise Exit
                end
            | BRecordGet(px, py) ->
                begin
                  match node with
                  | BRecordGet(nx, ny) ->
                      do_match_quoted ny py penv penv_len nenv nenv_len
                        (do_match_quoted nx px penv penv_len nenv nenv_len acc mode) mode
                  | _ -> raise Exit
                end
            | Delayed(r) ->
                begin
                  match node with
                  | Delayed(r0) -> do_match_quoted !r0 !r penv penv_len nenv nenv_len acc mode
                  | _ -> raise Exit
                end
            | Closure(pat2, env2, env2_len) ->
                do_match_quoted node pat2 env2 env2_len nenv nenv_len acc mode
            | LambdaClosure(body, env2, env2_len, call_type, times_entered, attrs) ->
                do_match_quoted node (Lambda(body, env2_len, call_type, times_entered, attrs)) env2 env2_len nenv nenv_len acc mode
            | _ ->
                Error.runtime_error ("bad pattern: " ^ Node.to_string pat)
        end
  in
  (* Debug.print ("do_match_quoted: " ^ Utils.list_to_string Node.to_string penv ^ " " ^ Node.to_string pat ^ ", " ^
               Utils.list_to_string Node.to_string nenv ^ " " ^ Node.to_string node); *)
  match node with
  | Var(n) ->
      if n >= nenv_len then
        raise Exit
      else
        begin
          let node2 = Env.nth nenv n
          in
          if node2 = Dummy then
            do_match_indeed ()
          else
            do_match_quoted node2 pat penv penv_len nenv nenv_len acc mode
        end
  | Closure(node2, env2, env2_len) ->
      do_match_quoted node2 pat penv penv_len env2 env2_len acc mode
  | LambdaClosure(body, env2, env2_len, call_type, times_entered, attrs) ->
      do_match_quoted (Lambda(body, env2_len, call_type, times_entered, attrs)) pat penv penv_len env2 env2_len acc mode
  | _ ->
      do_match_indeed ()

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
  | Tokens(x) ->
      begin
        match node with
        | Tokens(y) when check_tokens_eq x y -> acc
        | _ -> raise Exit
      end
  | Quoted(x) ->
      begin
        match node with
        | Quoted(y) -> do_match_quoted y x [] 0 [] 0 acc ModeMatch
        | _ -> raise Exit
      end
  | Appl(_) | Cond(_) | DynDef(_) | Delay(_) | Leave(_) | Force(_) | Var(_) | Proxy(_) | MakeRecord(_) |
    BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
    BConsNE(_) | BFst(_) | BSnd(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_) | BDynenvGet(_) |
    Closure(_) | Delayed(_) | Lambda(_) | Builtin(_) | LambdaClosure(_) | Dynenv(_)
    ->
      Error.runtime_error ("bad pattern: " ^ Node.to_string pat)
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

let xmatch = do_match

let equal node1 node2 =
  let x =
    List.hd
      (try
        do_match_quoted node1 node2 [] 0 [] 0 [True] ModeEq
      with Exit ->
        [False])
  in
  x == True

let equal_quoted node1 node2 =
  let x =
    List.hd
      (try
        do_match_quoted node1 node2 [] 0 [] 0 [True] ModeQuotedEq
      with Exit ->
        [False])
  in
  x == True
