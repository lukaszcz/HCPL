(* match.ml: Matching implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

open Node

exception Unknown

type match_quoted_mode_t = ModeMatch | ModeEq | ModeQuotedEq

let rec check_tokens_eq lst1 lst2 =
  match lst1, lst2 with
  | ((tok1, _) :: t1), ((tok2, _) :: t2) when Token.eq tok1 tok2 -> true
  | _ -> false

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
          (Node.mkquoted node) :: acc
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
      | Tokens(x) ->
          begin
            match node with
            | Tokens(y) -> if check_tokens_eq x y then acc else raise Exit
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
                  (fun a (n1, n2, n3, _) (p1, p2, p3, _) ->
                    do_match_quoted n3 p3 (do_match_quoted n2 p2 (do_match_quoted n1 p1 a mode) mode) mode)
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
          Error.runtime_error "bad pattern"
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
  | Tokens(x) ->
      begin
        match node with
        | Tokens(y) when check_tokens_eq x y -> acc
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
      Error.runtime_error "bad pattern"
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
        do_match_quoted node1 node2 [True] ModeEq
      with Exit ->
        [False])
  in
  x == True

let equal_quoted node1 node2 =
  let x =
    List.hd
      (try
        do_match_quoted node1 node2 [True] ModeQuotedEq
      with Exit ->
        [False])
  in
  x == True
