(* node.ml: Node type and related functions. Node.t is the type of a
   node in a program graph.

   Copyright (C) 2013 by Łukasz Czajka
*)

type call_t = CallByValue | CallByNeed | CallByName

type t =
  (* not immediate *)
  | Var of int
  | Proxy of t ref
  | Appl of t * t * attrs_t option (* a b -> Appl(a, b, attrs) *)
  | Cond of t * t * t * attrs_t option
  | Delay of t
  | Leave of t
  | Force of t
  | MakeRecord of t Symbol.Map.t

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
  | BAnd of t * t
  | BOr of t * t
  | BMatch of t * ((t * t * t * int) list)
        (* (value, [(pattern, when_condition, body, args_num)]) *)
  | BRecordGet of t * t

  (* used only by the evaluator *)
  | Closure of t * t list * int
  | Delayed of t ref

  (* immediate *)
  | Lambda of t * int * call_t * int ref * attrs_t option
        (* (body, frame number (for the argument), call type, times entered, attrs) *)
  | Builtin of (t list -> t) * int * attrs_t option
        (* (function, args num, attrs) *)
  | Integer of Big_int.big_int
  | String of string
  | Record of t Symbol.Map.t
  | Tokens of (Token.t * Lexing.position) list
  | Quoted of t

  (* used only by the evaluator *)
  | LambdaClosure of t * t list * int * call_t * int ref * attrs_t option
        (* (body, argument env, env_len, call type, times entered, attrs) *)

  (* immediate cntd. *)
  | Sym of Symbol.t
  | Cons of t * t

  (* UnboxedX are to ensure that the values of constant constructors have
     their second bit set (i.e. they are not divisible by 2 when
     interpreted as OCaml ints) *)
  | Unboxed1
  | Nil
  | Unboxed2
  | True
  | Unboxed3
  | False
  | Unboxed4
  | Placeholder
  | Unboxed5
  | Ignore
  | Unboxed6
  | Dummy
  | Unboxed7
  (* without the last UnboxedX value the compiler may generate wrong code *)

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
let progn = Lambda(id, 0, CallByValue, ref 0, Attrs.create None None)

let eq = Lambda(Lambda(BEq(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let gt = Lambda(Lambda(BGt(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let lt = Lambda(Lambda(BGt(Var(0), Var(1)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let ge = Lambda(Lambda(BGe(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let le = Lambda(Lambda(BGe(Var(0), Var(1)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let add = Lambda(Lambda(BAdd(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let sub = Lambda(Lambda(BSub(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let mul = Lambda(Lambda(BMul(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let idiv = Lambda(Lambda(BIDiv(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let xmod = Lambda(Lambda(BMod(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let cons = Lambda(Lambda(BCons(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let cons_comma = Lambda(Lambda(BCons(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let cons_dcolon = Lambda(Lambda(BCons(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let cons_lazy = Lambda(Lambda(BConsNE(Var(1), Var(0)), 1, CallByNeed, ref 0, None), 0, CallByNeed, ref 0, Attrs.create None None)
let xfst = Lambda(BFst(Var(0)), 0, CallByValue, ref 0, Attrs.create None None)
let xsnd = Lambda(BSnd(Var(0)), 0, CallByValue, ref 0, Attrs.create None None)
let xhd = Lambda(BFst(Var(0)), 0, CallByValue, ref 0, Attrs.create None None)
let xtl = Lambda(BSnd(Var(0)), 0, CallByValue, ref 0, Attrs.create None None)
let xand = Lambda(Lambda(BAnd(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)
let xor = Lambda(Lambda(BOr(Var(1), Var(0)), 1, CallByValue, ref 0, None), 0, CallByValue, ref 0, Attrs.create None None)

(* WARNING: some functions below depend on OCaml implementation details *)

let is_const (node : t) = Obj.is_int (Obj.repr node)

(* NOTE: The correctness of the matchings below depends on the fact
   that different constant constructors are not differentiated by the
   matches and the same should be returned for smallints as for
   constant constructors. *)
let is_immediate node = match node with
    (* note: do not use "| _ -> ..." here so that the compiler warns when we
       forget one of the possibilities *)
  | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Var(_) | Delayed(_) | Proxy(_) |
    MakeRecord(_) | Closure(_) |
    BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
    BConsNE(_) | BFst(_) | BSnd(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_)
    -> false
  | Lambda(_) | Builtin(_) | Integer(_) | String(_) | Record(_) | Sym(_) |
    True | False | Placeholder | Ignore | Cons(_) | Nil | Tokens(_) | Quoted(_) |
    LambdaClosure(_)| Dummy | Unboxed1 | Unboxed2 | Unboxed3 | Unboxed4 | Unboxed5 | Unboxed6 | Unboxed7
    -> true

(* true if node is immediate and closed, false if it _might_ not be *)
let is_immed node = match node with
    (* note: do not use "| _ -> ..." here so that the compiler warns when we
       forget one of the possibilities *)
  | Appl(_) | Cond(_) | Delay(_) | Force(_) | Leave(_) | Var(_) | Delayed(_) | Proxy(_) |
    MakeRecord(_) | Closure(_) | Lambda(_) | Builtin(_) |
    BEq(_) | BGt(_) | BGe(_) | BAdd(_) | BSub(_) | BMul(_) | BIDiv(_) | BMod(_) | BCons(_) |
    BConsNE(_) | BFst(_) | BSnd(_) | BAnd(_) | BOr(_) | BMatch(_) | BRecordGet(_)
    -> false
  | Integer(_) | String(_) | Record(_) | Sym(_) |
    True | False | Placeholder | Ignore | Cons(_) | Nil | Tokens(_) | Quoted(_) |
    LambdaClosure(_)| Dummy | Unboxed1 | Unboxed2 | Unboxed3 | Unboxed4 | Unboxed5 | Unboxed6 | Unboxed7
    -> true

let is_smallint (node : t) =
  if is_const node then
    (Obj.magic node) land 1 = 0
  else
    false

(* true if the node is closed, false if it _might_ not be *)
let is_closed (node : t) =
  if is_immed node then
    true
  else
    match node with
    | Lambda(_, 0, _, _, _) | Closure(_) -> true
    | _ -> false

let smallint_value (node : t) = assert (is_smallint node); (Obj.magic node) asr 1

let is_smallint_value v = v <= max_int asr 1 && v >= min_int asr 1

let make_smallint v = assert (is_smallint_value v); ((Obj.magic (v * 2)) : t)

let smallint_bits = Config.int_bits - 1

let rec get_attrs node =
  match node with
  | Appl(_, _, attrs) -> attrs
  | Cond(_, _, _, attrs) -> attrs
  | Delayed(rx) -> get_attrs !rx
  | Proxy(rx) -> get_attrs !rx
  | Lambda(_, _, _, _, attrs) -> attrs
  | LambdaClosure(_, _, _, _, _, attrs) -> attrs
  | Closure(x, _, _) -> get_attrs x
  | Builtin(_, _, attrs) -> attrs
  | _ -> None

let change_name node name =
  match get_attrs node with
  | Some(attrs) -> attrs.name <- Some(name)
  | None -> ()

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
  | Appl(Appl(f, x, _), y, _) when f == cons || f == cons_comma || f == cons_dcolon -> BCons(x, y)
  | Appl(f, x, _) when f == xfst || f == xhd -> BFst(x)
  | Appl(f, x, _) when f == xsnd || f == xtl -> BSnd(x)
  | Appl(Appl(f, x, _), y, _) when f == xand -> BAnd(x, y)
  | Appl(Appl(f, x, _), y, _) when f == xor -> BOr(x, y)
  | _ -> node1

let rec normalize node =
  match node with
  | Proxy(r) -> normalize !r
  | _ -> node

let mkquoted node =
  if is_const node then
    node
  else
    match node with
    | Integer(_) | String(_) | Sym(_) | Tokens(_) | Quoted(_) -> node
    | _ -> Quoted(node)

let is_quoted node =
  if is_const node then
    true
  else
    match node with
    | Integer(_) | String(_) | Sym(_) | Tokens(_) | Quoted(_) -> true
    | _ -> false

let unquote node =
  assert (is_quoted node);
  match node with
  | Quoted(x) -> x
  | _ -> node

let rec list_to_cons lst =
  match lst with
  | h :: t -> Cons(h, list_to_cons t)
  | [] -> Nil

let rec cons_to_list node =
  match node with
  | Cons(h, t) -> h :: (cons_to_list t)
  | Nil -> []
  | _ -> assert false

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
          | (pat, cond, value, _) :: t ->
              " | " ^ prn pat (limit - 1) ^ (if cond == True then "" else " when " ^ prn cond (limit - 1)) ^
              " -> " ^ prn value (limit - 1) ^ prn_match t
          | [] -> ""
        in
        let rec prn_progn node =
          match node with
          | Appl(Appl(f, x, _), y, _) when f == progn ->
              prn x (limit - 1) ^ "; " ^ prn_progn y
          | _ -> prn node (limit - 1)
        in
        let rec prn_tokens lst =
          match lst with
          | h :: t -> Token.to_string (fst h) ^ " " ^ prn_tokens t
          | [] -> ""
        in
        if is_smallint node then
          string_of_int (smallint_value node)
        else
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
          | Tokens(lst) -> "#< " ^ prn_tokens lst ^ ">#"
          | Quoted(x) -> "'" ^ prn x (limit - 1)
          | Placeholder -> "%%"
          | Ignore -> "%_"
          | Cons(x, y) ->
              if is_list y then
                prn_list node limit
              else
                "(" ^ prn x (limit - 1) ^ ", " ^ prn y (limit - 1) ^ ")"
          | Nil -> "()"
          | Delayed(_) -> "<delayed>"
          | Closure(body, env, _) ->
              "(closure " ^ Utils.list_to_string (fun x -> prn x (limit - 1)) env ^ ": " ^ prn body (limit - 1) ^ ")"
          | LambdaClosure(body, env, frm, call_type, _, attrs) ->
              "(closure " ^ Utils.list_to_string (fun x -> prn x (limit - 1)) env ^ ": " ^ lambda_str body frm attrs call_type ^ ")"
          | BEq(x, y) -> "(" ^ prn x (limit - 1) ^ " = " ^ prn y (limit - 1) ^ ")"
          | BGt(x, y) -> "(" ^ prn x (limit - 1) ^ " > " ^ prn y (limit - 1) ^ ")"
          | BGe(x, y) -> "(" ^ prn x (limit - 1) ^ " >= " ^ prn y (limit - 1) ^ ")"
          | BAdd(x, y) -> "(" ^ prn x (limit - 1) ^ " + " ^ prn y (limit - 1) ^ ")"
          | BSub(x, y) -> "(" ^ prn x (limit - 1) ^ " - " ^ prn y (limit - 1) ^ ")"
          | BMul(x, y) -> "(" ^ prn x (limit - 1) ^ " * " ^ prn y (limit - 1) ^ ")"
          | BIDiv(x, y) -> "(" ^ prn x (limit - 1) ^ " div " ^ prn y (limit - 1) ^ ")"
          | BMod(x, y) -> "(" ^ prn x (limit - 1) ^ " mod " ^ prn y (limit - 1) ^ ")"
          | BCons(x, y) -> "(cons " ^ prn x (limit - 1) ^ " " ^ prn y (limit - 1) ^ ")"
          | BConsNE(x, y) -> "(cons# " ^ prn x (limit - 1) ^ " " ^ prn y (limit - 1) ^ ")"
          | BFst(x) -> "(fst " ^ prn x (limit - 1) ^ ")"
          | BSnd(x) -> "(snd " ^ prn x (limit - 1) ^ ")"
          | BAnd(x, y) -> "(" ^ prn x (limit - 1) ^ " and " ^ prn y (limit - 1) ^ ")"
          | BOr(x, y) -> "(" ^ prn x (limit - 1) ^ " or " ^ prn y (limit - 1) ^ ")"
          | BMatch(x, branches) -> "(match " ^ prn x (limit - 1) ^ " with" ^ prn_match branches ^ ")"
          | BRecordGet(x, y) -> "(" ^ prn x (limit - 1) ^ "." ^ prn y (limit - 1) ^ ")"
          | Dummy -> "<dummy>"
          | Unboxed1 | Unboxed2 | Unboxed3 | Unboxed4 | Unboxed5 | Unboxed6 | Unboxed7 -> failwith "unknown node"
      end
  in
  prn node 20
