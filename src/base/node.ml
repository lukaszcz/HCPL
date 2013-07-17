(* node.ml: Node type and related functions. Node.t is the type of a
   node in a program graph.

   Copyright (C) 2013 by Łukasz Czajka
*)

type call_t = CallByValue | CallByNeed | CallByName

type t =
  (* not immediate *)
  | Appl of t * t * attrs_t option (* a b -> Appl(a, b, attrs) *)
  | Cond of t * t * t * attrs_t option
  | Delay of t
  | Leave of t
  | Force of t
  | Var of int
  | Proxy of t ref
  | MakeRecord of t Symbol.Map.t

  (* immediate *)
  | Lambda of t * int * call_t * int ref * attrs_t option
        (* (body, frame number (for the argument), call type, times entered, attrs) *)
  | Builtin of (t list -> t) * int * attrs_t option
        (* (function, args num, attrs) *)
  | Integer of Big_int.big_int
  | String of string
  | Record of t Symbol.Map.t
  | Sym of Symbol.t
  | Cons of t * t
  | Quoted of t
  | Nil
  | True
  | False
  | Placeholder
  | Ignore

  (* used only by the evaluator *)
  | Delayed of t ref
  | Closure of t * t list * int
(*  | LambdaClosure of t * t list * int * call_t * int ref * attrs_t option *)
        (* (body, argument env, env_len, call type, times entered, attrs) *)
and attrs_t = { name : Symbol.t option; pos : Lexing.position option;
                attr_map : (t Symbol.Map.t) option; node_type : t option }

let id = Lambda(Var(0), 0, CallByName, ref 0, None)
let progn = Lambda(id, 0, CallByValue, ref 0, None)

module Attrs =
  struct
    type t = attrs_t option

    let create aname apos =
      Some({ name = aname; pos = apos; attr_map = None; node_type = None })

    let get_name ma =
      match ma with
      | Some(a) -> a.name
      | None -> None

    let set_name ma aname =
      match ma with
      | Some(a) -> Some({ name = Some(aname); pos = a.pos; attr_map = a.attr_map;
                          node_type = a.node_type })
      | None -> Some({ name = Some(aname); pos = None; attr_map = None;
                       node_type = None })

    let get_pos ma =
      match ma with
      | Some(a) -> a.pos
      | None -> None

    let set_type ma atype =
      match ma with
      | Some(a) -> Some({ name = a.name; pos = a.pos; attr_map = a.attr_map;
                          node_type = Some(atype) })
      | None -> Some({ name = None; pos = None; attr_map = None;
                       node_type = Some(atype) })

    let get_attr ma name =
      match ma with
      | Some(a) ->
          begin
            match a.attr_map with
            | Some(map) when Symbol.Map.mem name map ->
                Some(Symbol.Map.find name map)
            | _ -> None
          end
      | None -> None

    let set_attr ma aname value =
      match ma with
      | Some(a) ->
          let map2 =
            match a.attr_map with
            | Some(map) ->
                Some(Symbol.Map.add aname value map)
            | None -> Some(Symbol.Map.add aname value (Symbol.Map.empty))
          in
          Some({ name = a.name; pos = a.pos; attr_map = map2;
                 node_type = a.node_type })
      | None -> Some({ name = None; pos = None;
                       attr_map = Some(Symbol.Map.add aname value (Symbol.Map.empty));
                       node_type = None })
  end

let alloc_data_tag =
  let tag = ref 100
  in
  (fun () -> incr tag; !tag)

let is_immediate = function
  (* note: don't use "| _ -> ..." here so that the compiler warns when we
  forget one of the possibilities *)
  | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Var(_) | Delayed(_) | Proxy(_) |
    MakeRecord(_) | Closure(_)
    -> false
  | Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
    True | False | Placeholder | Ignore | Cons(_) | Nil | Quoted(_)
    -> true

let rec get_attrs node =
  match node with
  | Appl(_, _, attrs) -> attrs
  | Cond(_, _, _, attrs) -> attrs
  | Delayed(rx) -> get_attrs !rx
  | Proxy(rx) -> get_attrs !rx
  | Lambda(_, _, _, _, attrs) -> attrs
  | Builtin(_, _, attrs) -> attrs
  | _ -> None

let get_name node = Attrs.get_name (get_attrs node)

let get_pos node = Attrs.get_pos (get_attrs node)

let get_attr node name = Attrs.get_attr (get_attrs node)

let rec is_module_closed node =
  match node with
  | Appl(Appl(f, x, _), y, _) when f == progn -> is_module_closed y
  | Appl(f, x, _) ->
      if f == id then
        is_module_closed x
      else
        false
  | MakeRecord(_) -> true
  | _ -> is_immediate node

let mkappl lst attrs =
  let rec loop lst attrs =
    match lst with
    | [x] -> x
    | h :: t -> Appl(loop t None, h, attrs)
    | [] -> assert false
  in
  loop (List.rev lst) attrs

(* Returns Nil for "don't know" *)
let rec equal node1 node2 =
  if not (is_immediate node1) || not (is_immediate node2) then
    Nil
  else
    match node1, node2 with
    | Lambda(_), Lambda(_) ->
        if node1 == node2 then
          True
        else
          Nil
    | Builtin(_), Builtin(_) ->
        if node1 == node2 then
          True
        else
          Nil
    | Integer(x), Integer(y) ->
        if Big_int.eq_big_int x y then
          True
        else
          False
    | String(x), String(y) ->
        if x = y then
          True
        else
          False
    | Record(x), Record(y) ->
        Nil (* TODO *)
    | Sym(x), Sym(y) ->
        if Symbol.eq x y then
          True
        else
          False
    | True, True -> True
    | False, False -> True
    | Placeholder, Placeholder -> True
    | Ignore, Ignore -> True
    | Cons(x1, y1), Cons(x2, y2) ->
        let res1 = equal x1 x2
        in
        if res1 = True then
          equal y1 y2
        else
          res1
    | Quoted(x), Quoted(y) ->
        equal x y
    | Nil, Nil -> True
    | _, _ -> False

let rec prune node =
  match node with
  | Appl(f, x, _) when f == id -> prune x
  | _ -> node

let call_type_to_string call_type =
  match call_type with
  | CallByValue -> "!"
  | CallByNeed -> "&"
  | CallByName -> "#"

let to_string node =
  let rec prn node limit =
    if limit = 0 then
      "..."
    else
      begin
        let lambda_str body frm attrs call_type =
          match Attrs.get_name attrs with
          | Some(name) -> Symbol.to_string name
          | None -> "\\" ^ (call_type_to_string call_type) ^ string_of_int frm ^ " " ^ prn body (limit - 1)
        in
        let rec is_list node =
          match node with
          | Cons(x, y) -> is_list y
          | Nil -> true
          | _ -> false
        in
        let prn_list node limit =
          let rec loop node limit2 =
            if limit2 > 0 then
              match node with
              | Cons(x, y) -> ", " ^ prn x (limit - 1) ^ loop y (limit2 - 1)
              | Nil -> ""
              | _ -> assert false
            else
              "..."
          in
          match node with
          | Cons(x, y) -> "[" ^ prn x (limit - 1) ^ loop y limit ^ "]"
          | _ -> assert false
        in
        let rec prn_progn node =
          match node with
          | Appl(Appl(f, x, _), y, _) when f == progn ->
              prn x (limit - 1) ^ "; " ^ prn_progn y
          | _ -> prn node (limit - 1)
        in
        match node with
        | Appl(Appl(f, x, _), y, _) when f == progn ->
            "{" ^ prn x (limit - 1) ^ "; " ^ prn_progn y ^ "}"
        | Appl(a, b, _) ->
            if a == id then
              "id(" ^ prn b (limit - 1) ^ ")"
            else
              "(" ^ (prn a (limit - 1)) ^ " " ^ (prn b (limit - 1)) ^ ")"
        | Cond(x, y, z, _) ->
            "(if " ^ (prn x (limit - 1)) ^ " then " ^ (prn y (limit - 1)) ^
            " else " ^ (prn z (limit - 1)) ^ ")"
        | Delay(x) -> "&" ^ prn x (limit - 1)
        | Force(x) -> "!" ^ prn x (limit - 1)
        | Leave(x) -> "#" ^ prn x (limit - 1)
        | Var(i) -> "$" ^ string_of_int i
        | Proxy(rx) -> prn !rx limit
        | MakeRecord(_) -> "<make-record>"
        | Lambda(body, frm, call_type, _, attrs) -> lambda_str body frm attrs call_type
        | Builtin(_) -> "<builtin>"
        | Integer(i) -> Big_int.string_of_big_int i
        | String(str) -> "\"" ^ (String.escaped str) ^ "\""
        | Record(_) -> "<record>"
        | Sym(sym) -> Symbol.to_string sym
        | True -> "true"
        | False -> "false"
        | Quoted(x) -> "(quote " ^ prn x (limit - 1) ^ ")"
        | Placeholder -> "%%"
        | Ignore -> "%_"
        | Cons(x, y) ->
            if is_list y then
              prn_list node limit
            else
              "(" ^ prn x (limit - 1) ^ ", " ^ prn y (limit - 1) ^ ")"
        | Nil -> "()"
        | Delayed(_) -> "<delayed>"
        | Closure(body, _, _) -> "(closure: " ^ prn body (limit - 1) ^ ")"
      end
  in
  prn node 20

let matches node pat =
  (* TODO: change (false, []) to raise Exit *)
  let rec aux node pat lst =
    match node with
    | Proxy(rn) -> aux !rn pat lst
    | _ -> if pat == node then (true, lst) else
      begin
        match pat with
        | Appl(x, y, _) ->
            begin
              match node with
              | Appl(a, b, _) ->
                  let (mb, lstb) = aux b y lst
                  in
                  if mb then
                    aux a x lstb
                  else
                    (false, [])
              | _ ->
                  (false, [])
            end
        | Cond(x, y, z, _) ->
            begin
              match node with
              | Cond(a, b, c, _) ->
                  let (mc, lstc) = aux c z lst
                  in
                  if mc then
                    let (mb, lstb) = aux b y lstc
                    in
                    if mb then
                      aux a x lstb
                    else
                      (false, [])
                  else
                    (false, [])
              | _ ->
                  (false, [])
            end
        | Delay(x) ->
            begin
              match node with
              | Delay(a) ->
                  aux a x lst
              | _ ->
                  (false, [])
            end
        | Leave(x) ->
            begin
              match node with
              | Leave(a) ->
                  aux a x lst
              | _ ->
                  (false, [])
            end
        | Force(x) ->
            begin
              match node with
              | Force(a) ->
                  aux a x lst
              | _ ->
                  (false, [])
            end
        | Integer(x) ->
            begin
              match node with
              | Integer(y) ->
                  if Big_int.eq_big_int x y then
                    (true, lst)
                  else
                    (false, [])
              | _ -> (false, [])
            end
        | Var(_) | MakeRecord(_) | String(_) | Record(_) | Sym(_) | Nil | True | False ->
            if node = pat then
              (true, lst)
            else
              (false, [])
        | Proxy(rx) ->
            aux node !rx lst
        | Lambda(body, frame, _, _, _) ->
            begin
              match node with
              | Lambda(body2, frame2, _, _, _) when frame = frame2 ->
                  aux body2 body lst
              | _ ->
                  (false, [])
            end
        | Placeholder ->
            (true, node :: lst)
        | Ignore ->
            (true, lst)
        | Cons(x, y) ->
            begin
              match node with
              | Cons(a, b) ->
                  let (mb, lstb) = aux b y lst
                  in
                  if mb then
                    aux a x lstb
                  else
                    (false, [])
              | _ ->
                  (false, [])
            end
        | Quoted(x) ->
            begin
              match node with
              | Quoted(a) ->
                  aux a x lst
              | _ ->
                  (false, [])
            end
        | _ ->
            failwith "bad pattern"
      end
  in
  aux node pat []
