(* scope.ml: Scope implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type identtab_t = (Node.t * int) Symbol.Map.t
      (* (node, scopenum) map *)

type t = { identtab : identtab_t; frame : int; scopenum : int;
           keywords : Symbol.Set.t; permanent_keywords : Symbol.Set.t;
           is_repl_mode : bool; mutable line_num : int }

exception Duplicate_ident

let empty = { identtab = Symbol.Map.empty;
              frame = -1; scopenum = 0;
              keywords = Symbol.Set.empty;
              permanent_keywords = Symbol.Set.empty;
              is_repl_mode = false;
              line_num = 0 }

let empty_repl = { identtab = Symbol.Map.empty;
                   frame = -1; scopenum = 0;
                   keywords = Symbol.Set.empty;
                   permanent_keywords = Symbol.Set.empty;
                   is_repl_mode = true;
                   line_num = 0 }

let push scope = { identtab = scope.identtab; frame = scope.frame;
                   scopenum = scope.scopenum + 1; keywords = Symbol.Set.empty;
                   permanent_keywords = scope.permanent_keywords;
                   is_repl_mode = scope.is_repl_mode;
                   line_num = scope.line_num
                 }

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
  { identtab = newtab; frame = scope.frame;
    scopenum = scope.scopenum; keywords = scope.keywords;
    permanent_keywords = scope.permanent_keywords; is_repl_mode = scope.is_repl_mode;
    line_num = scope.line_num
  }

let find_ident scope sym =
  let node = fst (Symbol.Map.find sym scope.identtab)
  in
  match node with
  | Node.Var(i) -> Node.Var(scope.frame - i)
  | _ -> node

let replace_ident scope sym node =
  let newtab = Symbol.Map.add sym (node, scope.scopenum) scope.identtab
  in
  { identtab = newtab; frame = scope.frame;
    scopenum = scope.scopenum; keywords = scope.keywords;
    permanent_keywords = scope.permanent_keywords;
    is_repl_mode = scope.is_repl_mode;
    line_num = scope.line_num
  }

let add_keyword scope sym =
  let kwds = Symbol.Set.add sym scope.keywords
  in
  { identtab = scope.identtab; frame = scope.frame;
    scopenum = scope.scopenum; keywords = kwds;
    permanent_keywords = scope.permanent_keywords;
    is_repl_mode = scope.is_repl_mode;
    line_num = scope.line_num
  }

let add_permanent_keyword scope sym =
  let kwds = Symbol.Set.add sym scope.permanent_keywords
  in
  { identtab = scope.identtab; frame = scope.frame;
    scopenum = scope.scopenum; keywords = scope.keywords;
    permanent_keywords = kwds; is_repl_mode = scope.is_repl_mode;
    line_num = scope.line_num
  }

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
  | Token.Newline -> scope.line_num <- scope.line_num + 1; Token.Sep
  | _ -> token

let strm_position scope strm =
  TokenStream.position (skip_whitespace scope strm)

let strm_next scope strm =
  TokenStream.next (skip_whitespace scope strm)

let is_strm_empty scope strm =
  TokenStream.is_empty (skip_whitespace scope strm)

let frame scope = scope.frame

let push_frame scope =
  { identtab = scope.identtab; frame = scope.frame + 1;
    scopenum = scope.scopenum; keywords = scope.keywords;
    permanent_keywords = scope.permanent_keywords;
    is_repl_mode = scope.is_repl_mode;
    line_num = scope.line_num
  }
