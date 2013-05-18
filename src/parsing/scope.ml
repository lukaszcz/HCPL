(* scope.ml: Scope implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type identtab_t = (Node.t * int) Symbol.Map.t
      (* (node, scopenum) map *)

type t = { identtab : identtab_t; frame : int; scopenum : int;
           keywords : Symbol.Set.t; permanent_keywords : Symbol.Set.t }

exception Duplicate_ident

let empty = { identtab = Symbol.Map.empty;
              frame = -1; scopenum = 0;
              keywords = Symbol.Set.empty;
              permanent_keywords = Symbol.Set.empty }

let push scope = { identtab = scope.identtab; frame = scope.frame;
                   scopenum = scope.scopenum + 1; keywords = Symbol.Set.empty;
                   permanent_keywords = scope.permanent_keywords
                 }

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
    permanent_keywords = scope.permanent_keywords
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
    permanent_keywords = scope.permanent_keywords
  }

let add_keyword scope sym =
  let kwds = Symbol.Set.add sym scope.keywords
  in
  { identtab = scope.identtab; frame = scope.frame;
    scopenum = scope.scopenum; keywords = kwds;
    permanent_keywords = scope.permanent_keywords
  }

let add_permanent_keyword scope sym =
  let kwds = Symbol.Set.add sym scope.permanent_keywords
  in
  { identtab = scope.identtab; frame = scope.frame;
    scopenum = scope.scopenum; keywords = scope.keywords;
    permanent_keywords = kwds
  }

let get_token scope strm =
  let token = TokenStream.token strm
  in
  match token with
  | Token.Symbol(sym) ->
      if Symbol.Set.mem sym scope.keywords || Symbol.Set.mem sym scope.permanent_keywords then
        Token.Keyword(sym)
      else
        token
  | _ -> token

let frame scope = scope.frame

let push_frame scope =
  { identtab = scope.identtab; frame = scope.frame + 1;
    scopenum = scope.scopenum; keywords = scope.keywords;
    permanent_keywords = scope.permanent_keywords
  }
