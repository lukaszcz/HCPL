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
  | LambdaEager of t * int * int ref * attrs_t option
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

  (* inlined core builtins *)
  | BEq of t * t
  | BGt of t * t
  | BGe of t * t
  | BAdd of t * t
  | BSub of t * t
  | BMul of t * t
  | BIDiv of t * t
  | BMod of t * t
  | BCons of t * t
  | BConsNE of t * t
  | BFst of t
  | BSnd of t
  | BNot of t
  | BAnd of t * t
  | BOr of t * t
  | BMatch of t * ((t * t * int) list)
  | BRecordGet of t * t

  (* used only by the evaluator *)
  | Delayed of t ref
  | Closure of t * t list * int
  | LambdaClosure of t * t list * int * call_t * int ref * attrs_t option
        (* (body, argument env, env_len, call type, times entered, attrs) *)
  | LambdaEagerClosure of t * t list * int * int ref * attrs_t option
        (* (body, argument env, env_len, times entered, attrs) *)
and attrs_t = { mutable name : Symbol.t option; pos : Lexing.position option;
                attr_map : (t Symbol.Map.t) option; node_type : t option }

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
      | Some(a) -> Some({ a with name = Some(aname) })
      | None -> Some({ name = Some(aname); pos = None; attr_map = None;
                       node_type = None })

    let get_pos ma =
      match ma with
      | Some(a) -> a.pos
      | None -> None

    let set_type ma atype =
      match ma with
      | Some(a) -> Some({ a with node_type = Some(atype) })
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
          Some({ a with attr_map = map2 })
      | None -> Some({ name = None; pos = None;
                       attr_map = Some(Symbol.Map.add aname value (Symbol.Map.empty));
                       node_type = None })
  end

let id = Lambda(Var(0), 0, CallByName, ref 0, Attrs.create None None)
let progn = LambdaEager(id, 0, ref 0, Attrs.create None None)

let eq = LambdaEager(LambdaEager(BEq(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let gt = LambdaEager(LambdaEager(BGt(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let lt = LambdaEager(LambdaEager(BGt(Var(0), Var(1)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let ge = LambdaEager(LambdaEager(BGe(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let le = LambdaEager(LambdaEager(BGe(Var(0), Var(1)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let add = LambdaEager(LambdaEager(BAdd(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let sub = LambdaEager(LambdaEager(BSub(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let mul = LambdaEager(LambdaEager(BMul(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let idiv = LambdaEager(LambdaEager(BIDiv(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let xmod = LambdaEager(LambdaEager(BMod(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let cons = LambdaEager(LambdaEager(BCons(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let cons_comma = LambdaEager(LambdaEager(BCons(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let cons_lazy = Lambda(Lambda(BConsNE(Var(1), Var(0)), 1, CallByNeed, ref 0, None), 0, CallByNeed, ref 0, Attrs.create None None)
let xfst = LambdaEager(BFst(Var(0)), 0, ref 0, Attrs.create None None)
let xsnd = LambdaEager(BSnd(Var(0)), 0, ref 0, Attrs.create None None)
let xhd = LambdaEager(BFst(Var(0)), 0, ref 0, Attrs.create None None)
let xtl = LambdaEager(BSnd(Var(0)), 0, ref 0, Attrs.create None None)
let xnot = LambdaEager(BNot(Var(0)), 0, ref 0, Attrs.create None None)
let xand = LambdaEager(LambdaEager(BAnd(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)
let xor = LambdaEager(LambdaEager(BOr(Var(1), Var(0)), 1, ref 0, None), 0, ref 0, Attrs.create None None)

let is_immediate = function
  (* note: don't use "| _ -> ..." here so that the compiler warns when we
  forget one of the possibilities *)
  | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Var(_) | Delayed(_) | Proxy(_) |
    MakeRecord(_) | Closure(_)
    -> false
  | Lambda(_) | LambdaEager(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
    True | False | Placeholder | Ignore | Cons(_) | Nil | Quoted(_) |
    LambdaClosure(_) | LambdaEagerClosure(_) |
    BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
    BConsNE(_) | BFst(_) | BSnd(_) | BNot(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_)
    -> true

let rec get_attrs node =
  match node with
  | Appl(_, _, attrs) -> attrs
  | Cond(_, _, _, attrs) -> attrs
  | Delayed(rx) -> get_attrs !rx
  | Proxy(rx) -> get_attrs !rx
  | Lambda(_, _, _, _, attrs) -> attrs
  | LambdaEager(_, _, _, attrs) -> attrs
  | LambdaClosure(_, _, _, _, _, attrs) -> attrs
  | LambdaEagerClosure(_, _, _, _, attrs) -> attrs
  | Closure(x, _, _) -> get_attrs x
  | Builtin(_, _, attrs) -> attrs
  | _ -> None

let change_name node name =
  match get_attrs node with
  | Some(attrs) -> attrs.name <- Some(name)
  | None -> assert false

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
    | LambdaEager(_), LambdaEager(_) ->
        if node1 == node2 then
          True
        else
          Nil
    | LambdaClosure(_), LambdaClosure(_) ->
        if node1 == node2 then
          True
        else
          Nil
    | LambdaEagerClosure(_), LambdaEagerClosure(_) ->
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

let optimize node =
  let node1 = prune node
  in
  match node1 with
  | Appl(Appl(f, x, _), y, _) when f == eq -> BEq(x, y)
  | Appl(Appl(f, x, _), y, _) when f == gt -> BGt(x, y)
  | Appl(Appl(f, x, _), y, _) when f == lt -> BGt(y, x)
  | Appl(Appl(f, x, _), y, _) when f == ge -> BGe(x, y)
  | Appl(Appl(f, x, _), y, _) when f == le -> BGe(y, x)
  | Appl(Appl(f, x, _), y, _) when f == add -> BAdd(x, y)
  | Appl(Appl(f, x, _), y, _) when f == sub -> BSub(x, y)
  | Appl(Appl(f, x, _), y, _) when f == mul -> BMul(x, y)
  | Appl(Appl(f, x, _), y, _) when f == idiv -> BIDiv(x, y)
  | Appl(Appl(f, x, _), y, _) when f == xmod -> BMod(x, y)
  | Appl(Appl(f, x, _), y, _) when f == cons || f == cons_comma -> BCons(x, y)
  | Appl(f, x, _) when f == xfst || f == xhd -> BFst(x)
  | Appl(f, x, _) when f == xsnd || f == xtl -> BSnd(x)
  | Appl(f, x, _) when f == xnot -> BNot(x)
  | Appl(Appl(f, x, _), y, _) when f == xand -> BAnd(x, y)
  | Appl(Appl(f, x, _), y, _) when f == xor -> BOr(x, y)
  | _ -> node1

let rec normalize node =
  match node with
  | LambdaEager(body, frame, seen, attrs) -> Lambda(body, frame, CallByValue, seen, attrs)
  | Proxy(r) -> normalize !r
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
        let rec prn_match lst =
          match lst with
          | (pat, value, _) :: t ->
              " | " ^ prn pat (limit - 1) ^ " -> " ^ prn value (limit - 1) ^ prn_match t
          | [] -> ""
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
        | LambdaEager(body, frm, _, attrs) -> lambda_str body frm attrs CallByValue
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
        | LambdaClosure(body, _, frm, call_type, _, attrs) -> lambda_str body frm attrs call_type
        | LambdaEagerClosure(body, _, frm, _, attrs) -> lambda_str body frm attrs CallByValue
        | BEq(x, y) -> prn x (limit - 1) ^ " = " ^ prn y (limit - 1)
        | BGt(x, y) -> prn x (limit - 1) ^ " > " ^ prn y (limit - 1)
        | BGe(x, y) -> prn x (limit - 1) ^ " >= " ^ prn y (limit - 1)
        | BAdd(x, y) -> prn x (limit - 1) ^ " + " ^ prn y (limit - 1)
        | BSub(x, y) -> prn x (limit - 1) ^ " - " ^ prn y (limit - 1)
        | BMul(x, y) -> prn x (limit - 1) ^ " * " ^ prn y (limit - 1)
        | BIDiv(x, y) -> prn x (limit - 1) ^ " div " ^ prn y (limit - 1)
        | BMod(x, y) -> prn x (limit - 1) ^ " mod " ^ prn y (limit - 1)
        | BCons(x, y) -> "(cons " ^ prn x (limit - 1) ^ " " ^ prn y (limit - 1) ^ ")"
        | BConsNE(x, y) -> "(cons# " ^ prn x (limit - 1) ^ " " ^ prn y (limit - 1) ^ ")"
        | BFst(x) -> "(fst " ^ prn x (limit - 1) ^ ")"
        | BSnd(x) -> "(snd " ^ prn x (limit - 1) ^ ")"
        | BNot(x) -> "(not " ^ prn x (limit - 1) ^ ")"
        | BAnd(x, y) -> prn x (limit - 1) ^ " and " ^ prn y (limit - 1)
        | BOr(x, y) -> prn x (limit - 1) ^ " or " ^ prn y (limit - 1)
        | BMatch(x, branches) -> "(match " ^ prn x (limit - 1) ^ " with" ^ prn_match branches ^ ")"
        | BRecordGet(x, y) -> prn x (limit - 1) ^ "." ^ prn y (limit - 1)
      end
  in
  prn node 20
