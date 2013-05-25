(* oper.ml: Operator rewriting implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type oper_t = { f_prio : int; f_assoc : int; f_arity : int; f_node : Node.t }

type t = oper_t Symbol.Map.t * int (* min prio *) * int (* max prio *)
type prio_t = After of Symbol.t | Before of Symbol.t | Equal of Symbol.t |
              AfterAppl | BeforeAppl | EqualAppl | First | Last

let assoc_left = 0
let assoc_right = 1

(* ------------------------------------------------------------------------ *)
(* private *)

let create prio assoc arity = { f_prio = prio; f_assoc = assoc; f_arity = arity; f_node = Node.Nil }

let prio op = op.f_prio

let assoc op = op.f_assoc

let arity op = op.f_arity

let node op = op.f_node

(* ------------------------------------------------------------------------ *)
(* public *)

let empty = (Symbol.Map.empty, 0, 0)

let add (opertab, min_prio, max_prio) sym aprio assoc arity =
  match aprio with
  | After(sym2) ->
      let oper = Symbol.Map.find sym2 opertab
      in
      let tab2 =
        Symbol.Map.map (fun op -> if prio op > prio oper then { op with f_prio = op.f_prio + 1 } else op) opertab
      in
      (Symbol.Map.add sym (create (prio oper + 1) assoc arity) tab2, min_prio, max_prio)

  | Before(sym2) ->
      let oper = Symbol.Map.find sym2 opertab
      in
      let tab2 =
        Symbol.Map.map (fun op -> if prio op < prio oper then { op with f_prio = op.f_prio - 1 } else op) opertab
      in
      (Symbol.Map.add sym (create (prio oper - 1) assoc arity) tab2, min_prio, max_prio)

  | Equal(sym2) ->
      let oper = Symbol.Map.find sym2 opertab
      in
      (Symbol.Map.add sym (create (prio oper) assoc arity) opertab, min_prio, max_prio)

  | AfterAppl ->
      let tab2 =
        Symbol.Map.map (fun op -> if prio op > 0 then { op with f_prio = op.f_prio + 1 } else op) opertab
      in
      (Symbol.Map.add sym (create 1 assoc arity) tab2, min_prio, max max_prio 1)

  | BeforeAppl ->
      let tab2 =
        Symbol.Map.map (fun op -> if prio op < 0 then { op with f_prio = op.f_prio - 1 } else op) opertab
      in
      (Symbol.Map.add sym (create (-1) assoc arity) tab2, min min_prio (-1), max_prio)

  | EqualAppl ->
      (Symbol.Map.add sym (create 0 assoc arity) opertab, min_prio, max_prio)

  | First ->
      (Symbol.Map.add sym (create (min_prio - 1) assoc arity) opertab, min_prio - 1, max_prio)

  | Last ->
      (Symbol.Map.add sym (create (min_prio + 1) assoc arity) opertab, min_prio, max_prio + 1)

let drop (opertab, min_prio, max_prio) sym =
  (Symbol.Map.remove sym opertab, min_prio, max_prio)

let rewrite (opertab, _, _) lst =
  let build_appl lst =
    let rec build lst =
      match lst with
      | [x] -> node x
      | x :: t -> Node.Appl(build t, node x, None)
      | _ -> assert false
    in
    build (List.rev lst)
  in
  let appl_op =
    { f_prio = 0; f_assoc = assoc_left; f_arity = 0; f_node = Node.Nil }
  in
  let create_from_op op node =
    { f_prio = prio op; f_assoc = assoc op; f_arity = arity op; f_node = node }
  in
  let create_appl lst = create_from_op appl_op (build_appl lst)
  in
  let mkopers lst =
    List.map
      (fun x ->
        match Node.get_name x with
        | Some(sym) ->
            begin
              try
                let op = Symbol.Map.find sym opertab
                in
                create_from_op op x
              with Not_found ->
                create_from_op appl_op x
            end
        | None -> create_from_op appl_op x)
      lst
  in
  (* the algorithm works in linear time *)
  let rec do_rewrite lst =
    build_appl (rewrite_binary false (rewrite_appl (rewrite_binary true (rewrite_unary lst))))

  and rewrite_unary lst =
    let mkunary op args =
      create_appl [op; create_appl (rewrite_binary false (rewrite_appl (rewrite_binary true args)))]
    in
    let rec reduce left op acc right =
      match right with
      | h :: t ->
          if (prio op) <= (prio h) then
            let (acc2, right2) =
              if acc = [] then
                ([h], t)
              else
                (acc, right)
            in
            let app = mkunary op (List.rev acc2)
            in
            find_unary (app :: left) right2
          else if arity h = 1 then
            reduce left op acc (rewrite_unary right)
          else
            reduce left op (h :: acc) t
      | [] ->
          List.rev ((mkunary op (List.rev acc)) :: left)
    and find_unary left right =
      match right with
      | h :: t ->
          if arity h = 1 then
            reduce left h [] t
          else
            find_unary (h :: left) t
      | [] ->
          List.rev left
    in
    find_unary [] lst

  and rewrite_appl lst =
    let rec shift left right =
      match right with
      | h :: t ->
          if prio h <> 0 then
            shift (h :: left) t
          else
            gather left [h] t
      | [] ->
          List.rev left
    and gather left acc right =
      match right with
      | h :: t ->
          if prio h = 0 then
            gather left (h :: acc) t
          else
            shift ((create_appl (List.rev acc)) :: left) right
      | [] ->
          List.rev ((create_appl (List.rev acc)) :: left)
    in
    shift [] lst

  and rewrite_binary f_before_appl lst =
    (* rewrite_binary works in linear time (simple amortized analysis) *)
    let rec shift left op rarg right =
      match right with
      | h :: t ->
          if (prio op) + (assoc op) > (prio h) then
            match t with
            | h2 :: t2 ->
                shift (rarg :: op :: left) h h2 t2
            | [] ->
                reduce left op rarg right
          else
            reduce left op rarg right
      | [] ->
          reduce left op rarg right
    and reduce left op rarg right =
      match left with
      | larg :: lt ->
          begin
            let app = create_appl [op; larg; rarg]
            in
            match lt with
            | h :: t ->
                skip t h (app :: right)
            | [] ->
                begin
                  match right with
                  | h :: t ->
                      skip [app] h t
                  | [] ->
                      [app]
                end
          end
      | [] ->
          assert false
    and skip left cur right =
      match right with
      | h :: t ->
          if prio cur = 0 || (prio cur > 0 && f_before_appl) then
            skip (cur :: left) h t
          else
            shift left cur h t
      | [] ->
          List.rev (cur :: left)
    in
    match lst with
    | x :: y :: z :: t ->
        if f_before_appl then
          skip [x] y (z :: t)
        else
          shift [x] y z t
    | _ -> lst
  in
  do_rewrite (mkopers lst)
