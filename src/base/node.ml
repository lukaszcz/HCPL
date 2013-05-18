(* node.ml: Node type and related functions. Node.t is the type of a
   node in a program graph.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t =
  (* not immediate *)
  | Progn of t list * attrs_t option
  | Appl of t * t * attrs_t option (* a b ~> Appl(a, b, attrs) *)
  | Cond of t * t * t * attrs_t option
  | Delay of t
  | Force of t
  | Var of int
  | Fail of t * attrs_t option
  | Proxy of t ref

  (* immediate *)
  | Lambda of t * int * int ref * attrs_t option
        (* (body, frame number, times entered, attrs) *)
  | LambdaEager of t * int * int ref * attrs_t option
        (* (body, frame number, times entered, attrs) *)
  | Builtin of (t list -> t) * int * attrs_t option
        (* (function, args num, attrs) *)
  | Integer of Big_int.big_int
  | True
  | False
  | Cons of t * t
  | Nil
  | Data of int * t list (* (tag, list of arguments) *)
  | Error of t * attrs_t option

  (* used only by the evaluator *)
  | Delayed of t ref
  | Closure of t * t list * int
  | ChangeStackEnv of t list * int
  | Store of t ref
  | ReturnProgn of t list
  | ReturnApply of t * t list * int
  | ReturnCond of t * t
and attrs_t = { name : Symbol.t option; pos : Lexing.position option;
                attr_map : (t Symbol.Map.t) option; node_type : t option }

module Attrs =
  struct
    type t = attrs_t option

    let create aname apos =
      Some({ name = aname;  pos = apos; attr_map = None; node_type = None })

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
  | Progn(_) | Appl(_) | Cond(_) | Delay(_) | Force(_) | Var(_) | Fail(_) | Delayed(_) | Proxy(_) |
    Closure(_)
    -> false
  | Lambda(_) | LambdaEager(_) | Builtin(_) | Integer(_) | True | False | Cons(_) |
    Nil | Data(_) | Error(_)
    -> true
  | ChangeStackEnv(_) | Store(_) | ReturnProgn(_) | ReturnApply(_) | ReturnCond(_)
    -> false (* whatever... *)

let rec get_attrs node =
  match node with
  | Progn(_, attrs) -> attrs
  | Appl(_, _, attrs) -> attrs
  | Cond(_, _, _, attrs) -> attrs
  | Fail(_, attrs) -> attrs
  | Delayed(rx) -> get_attrs !rx
  | Proxy(rx) -> get_attrs !rx
  | Lambda(_, _, _, attrs) -> attrs
  | LambdaEager(_, _, _, attrs) -> attrs
  | Builtin(_, _, attrs) -> attrs
  | Error(_, attrs) -> attrs
  | _ -> None

let get_name node = Attrs.get_name (get_attrs node)

let get_pos node = Attrs.get_pos (get_attrs node)

let get_attr node name = Attrs.get_attr (get_attrs node)

(* Returns Nil for "don't know" *)
let rec equal node1 node2 =
  if not (is_immediate node1) || not (is_immediate node2) then
    Nil
  else
    match node1, node2 with
    | Lambda(_, _, _, _), Lambda(_, _, _, _) ->
        if node1 == node2 then
          True
        else
          Nil
    | LambdaEager(_, _, _, _), LambdaEager(_, _, _, _) ->
        if node1 == node2 then
          True
        else
          Nil
    | Builtin(_, _, _), Builtin(_, _, _) ->
        if node1 == node2 then
          True
        else
          Nil
    | Integer(x), Integer(y) ->
        if Big_int.eq_big_int x y then
          True
        else
          False
    | True, True -> True
    | False, False -> False
    | Cons(x1, y1), Cons(x2, y2) ->
        let res1 = equal x1 x2
        in
        if res1 = True then
          equal y1 y2
        else
          res1
    | Nil, Nil -> True
    | Data(_, _), Data(_, _) -> Nil (* TODO *)
    | Error(_, _), Error(_, _) -> Nil
    | _, _ -> False

let to_string node =
  let rec prn node limit =
    if limit = 0 then
      "..."
    else
      begin
        let lambda_str body frm attrs is_eager =
          match Attrs.get_name attrs with
          | Some(name) -> Symbol.to_string name
          | None -> "\\" ^ (if is_eager then "!" else "&") ^ string_of_int frm ^ " " ^ prn body (limit - 1)
        in
        match node with
        | Progn(lst, _) -> "{" ^ (List.fold_left (fun acc x -> acc ^ prn x (limit - 1) ^ "; ") "" lst) ^ "}"
        | Appl(a, b, _) -> "(" ^ (prn a (limit - 1)) ^ " " ^ (prn b (limit - 1)) ^ ")"
        | Cond(x, y, z, _) ->
            "(if " ^ (prn x (limit - 1)) ^ " then " ^ (prn y (limit - 1)) ^
            " else " ^ (prn z (limit - 1)) ^ ")"
        | Delay(x) -> "&" ^ prn x (limit - 1)
        | Force(x) -> "!" ^ prn x (limit - 1)
        | Var(i) -> "$" ^ string_of_int i
        | Fail(_) -> "<fail>"
        | Proxy(rx) -> prn !rx limit
        | Lambda(body, frm, _, attrs) -> lambda_str body frm attrs false
        | LambdaEager(body, frm, _, attrs) -> lambda_str body frm attrs true
        | Builtin(_) -> "<builtin>"
        | Integer(i) -> Big_int.string_of_big_int i
        | True -> "true"
        | False -> "false"
        | Cons(x, y) -> "(cons " ^ prn x (limit - 1) ^ " " ^ prn y (limit - 1) ^ ")"
        | Nil -> "()"
        | Data(_) -> "<data>"
        | Error(_) -> "<error>"
        | Delayed(_) -> "<delayed>"
        | Closure(body, _, _) -> "(closure: " ^ prn body (limit - 1) ^ ")"
        | ChangeStackEnv(_) -> "<change-stack-env>"
        | Store(_) -> "<store>"
        | ReturnProgn(_) -> "<return-progn>"
        | ReturnApply(_) -> "<return-apply>"
        | ReturnCond(_) -> "<return-cond>"
      end
  in
  prn node 20
