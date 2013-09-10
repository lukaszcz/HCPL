(* scope.ml: Scope implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type identtab_t = (Node.t * int) Symbol.Map.t
      (* (node, scopenum) map *)
type syntaxlst_t = (Syntax.t * int) list
      (* (syntax list, scopenum) map *)

type t = { identtab : identtab_t; frame : int; scopenum : int;
           keywords : Symbol.Set.t; permanent_keywords : Symbol.Set.t;
           is_repl_mode : bool; opertab : Opertab.t;
           modules : Symbol.t list; module_mode : bool;
           placeholders : Symbol.t list ref; match_mode : bool;
           blocks : Symbol.t Symbol.Map.t;
           syntaxlst : syntaxlst_t;
           mutable line_num : int }

exception Duplicate_ident
exception Circular_dependency of Symbol.t list

let empty = { identtab = Symbol.Map.empty;
              frame = -1; scopenum = 0;
              keywords = Symbol.Set.empty;
              permanent_keywords = Symbol.Set.empty;
              is_repl_mode = false;
              opertab = Opertab.empty;
              modules = [];
              module_mode = false;
              placeholders = ref [];
              match_mode = false;
              blocks = Symbol.Map.empty;
              syntaxlst = [];
              line_num = 0 }

let empty_repl = { empty with is_repl_mode = true }

let push scope = { scope with scopenum = scope.scopenum + 1; module_mode = false; keywords = Symbol.Set.empty }

let push_ident_scope scope = { scope with scopenum = scope.scopenum + 1; module_mode = false }

let nesting scope = scope.scopenum

let add_ident scope sym node =
  let newtab =
    try
      let (_, scnum) = Symbol.Map.find sym scope.identtab
      in
      assert (scnum <= scope.scopenum);
      if scnum < scope.scopenum then
        Symbol.Map.add sym (node, scope.scopenum) scope.identtab
      else
        raise Duplicate_ident
    with
      Not_found -> Symbol.Map.add sym (node, scope.scopenum) scope.identtab
  in
  { scope with identtab = newtab }

let find_ident scope sym =
  let node = fst (Symbol.Map.find sym scope.identtab)
  in
  match node with
  | Node.Var(i) -> Node.Var(scope.frame - i)
  | _ -> node

let replace_ident scope sym node =
  let newtab = Symbol.Map.add sym (node, scope.scopenum) scope.identtab
  in
  { scope with identtab = newtab }

let drop_ident scope sym =
  let newtab = Symbol.Map.remove sym scope.identtab
  in
  { scope with identtab = newtab }

let identtab scope =
  Symbol.Map.fold
    (fun sym (node, scnum) acc ->
      if scnum <> scope.scopenum then
        acc
      else
        Symbol.Map.add sym node acc
    )
    scope.identtab
    Symbol.Map.empty

let enter_module scope (sym : Symbol.t) =
  let rec loop lst acc =
    match lst with
    | h :: t ->
        if Symbol.eq h sym then
          raise (Circular_dependency (h :: acc))
        else
          loop t (h :: acc)
    | [] ->
        { scope with modules = sym :: scope.modules; module_mode = true }
  in
  loop scope.modules []

let leave_module scope =
  match scope.modules with
  | h :: t -> { scope with modules = t; module_mode = false }
  | [] -> scope

let current_module scope =
  match scope.modules with
  | h :: _ -> h
  | [] -> Symbol.empty

let is_module_mode scope =
  scope.module_mode

let enter_match scope =
  { scope with placeholders = ref []; match_mode = true }

let add_placeholder scope sym =
  scope.placeholders := sym :: !(scope.placeholders);
  scope

let placeholders scope =
  List.rev !(scope.placeholders)

let is_match_mode scope =
  scope.match_mode

let add_keyword scope sym =
  let kwds = Symbol.Set.add sym scope.keywords
  in
  { scope with keywords = kwds }

let add_permanent_keyword scope sym =
  let kwds = Symbol.Set.add sym scope.permanent_keywords
  in
  { scope with permanent_keywords = kwds }

let lineno scope = scope.line_num

let rec skip_whitespace scope strm =
  if (scope.is_repl_mode && scope.scopenum = 0) || TokenStream.is_empty strm then
    strm
  else
    match TokenStream.token strm with
    | Token.Newline -> skip_whitespace scope (TokenStream.next strm)
    | _ -> strm

let strm_token scope strm =
  let strm = skip_whitespace scope strm
  in
  let token = TokenStream.token strm
  in
  match token with
  | Token.Symbol(sym) ->
      if Symbol.Set.mem sym scope.keywords || Symbol.Set.mem sym scope.permanent_keywords then
        Token.Keyword(sym)
      else
        token
  | Token.Newline | Token.NewlineSep -> scope.line_num <- scope.line_num + 1; Token.Sep
  | _ -> token

let strm_position scope strm =
  TokenStream.position (skip_whitespace scope strm)

let strm_next scope strm =
  TokenStream.next (skip_whitespace scope strm)

let is_strm_empty scope strm =
  TokenStream.is_empty (skip_whitespace scope strm)

let frame scope = scope.frame

let push_frame scope =
  { scope with frame = scope.frame + 1 }

let pop_frame scope =
  { scope with frame = scope.frame - 1 }

let rewrite scope lst =
  Opertab.rewrite scope.opertab lst

let add_oper scope sym prio assoc arity =
  let tab = Opertab.add scope.opertab sym prio assoc arity
  in
  let slst = (Syntax.Oper(sym, prio, assoc, arity), scope.scopenum) :: scope.syntaxlst
  in
  { scope with opertab = tab; syntaxlst = slst }

let drop_oper scope sym =
  let tab = Opertab.drop scope.opertab sym
  in
  let slst =
    List.filter
      (fun x ->
        match x with
        | (Syntax.Oper(opsym, _, _, _), _) when Symbol.eq sym opsym -> false
        | _ -> true)
      scope.syntaxlst
  in
  { scope with opertab = tab; syntaxlst = slst }

let add_block scope beg_sym end_sym =
  let blocks2 = Symbol.Map.add beg_sym end_sym scope.blocks
  and kwds2 = Symbol.Set.add end_sym (Symbol.Set.add beg_sym scope.permanent_keywords)
  in
  let slst = (Syntax.Block(beg_sym, end_sym), scope.scopenum) :: scope.syntaxlst
  in
  { scope with blocks = blocks2; permanent_keywords = kwds2; syntaxlst = slst }

let get_block_end scope beg_sym =
  Symbol.Map.find beg_sym scope.blocks

let get_syntax scope =
  List.fold_left
    (fun acc (s, scnum) ->
      if scnum <> scope.scopenum then
        acc
      else
        s :: acc
    )
    []
    scope.syntaxlst

let rec add_syntax scope lst =
  match lst with
  | Syntax.Oper(sym, prio, assoc, arity) :: t ->
      add_syntax (add_oper scope sym prio assoc arity) t
  | Syntax.Block(beg_sym, end_sym) :: t ->
      add_syntax (add_block scope beg_sym end_sym) t
  | [] -> scope
