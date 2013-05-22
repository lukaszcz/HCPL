(* parser.ml: Parser implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

(* -------------------------------------------------------------------------- *)
(* Parser rules implementation *)

(* The parser rules return lists of nodes (plus actually a state, but
   this is an implementation detail). The parser rules may be built up
   from basic parser rules using ++, ||| and >>.

   rule1 ++ rule2

   means: return the concatenation of the results of rule1 and rule2
   retaining changes to scope (table of identifiers) made inside rule1
   and rule2

   rule1 ||| rule2

   means: try performing rule1; if successful, return the result of
   rule1; if rule1 fails then return the result of rule2

   rule >> action

   means: save the current scope; evaluate rule; then perform action
   on the result of rule, returning a singleton list containing the
   result of the action; finally, restore the previous scope,
   i.e. forget all changes to scope (table of identifiers) made inside
   rule

   Note that the priority of the operators from most tightly binding
   to least tightly binding is: ++, |||, >>. Hence e.g.

   rule1 ++ rule2 ||| rule3 >> action

   is equivalent to

   ((rule1 ++ rule2) ||| rule3) >> action

   You may also use +> which is equivalent to >>, but has the priority
   of ++.

   The basic parser rules and some additional rule building functions
   are described in comments next to corresponding definitions
   below. *)

type sexp_t =
  | Program of Node.t
  | Ident of Symbol.t
  | Bool of bool
  | Number of Big_int.big_int
  | Sexp of sexp_t list

(* pretty printing *)
let rec sexp_to_string sexp =
  match sexp with
  | Program(node) -> "Program(" ^ Node.to_string node ^ ")"
  | Ident(sym) -> "Ident(" ^ Symbol.to_string sym ^ ")"
  | Bool(b) -> "Bool(" ^ (if b then "true" else "false") ^ ")"
  | Number(num) -> "Number(" ^ Big_int.string_of_big_int num ^ ")"
  | Sexp(lst) -> "Sexp(" ^ sexp_list_to_string lst ^ ")"
and sexp_list_to_string lst = Utils.list_to_string sexp_to_string lst

let new_attrs scope strm =
  let pos = if Scope.is_strm_empty scope strm then None else Some(Scope.strm_position scope strm)
  in
  Node.Attrs.create None pos

module State =
  struct
    type t = sexp_t list * Node.Attrs.t * TokenStream.t * Scope.t

    let get_lst ((x, _, _, _) : t) = x
    let get_attrs ((_, x, _, _) : t) = x
    let get_strm ((_, _, x, _) : t) = x
    let get_scope ((_, _, _, x) : t) = x
  end

type parser_cont_t = State.t -> State.t
type parser_rule_t = unit -> State.t -> parser_cont_t -> State.t
type parser_action_t = sexp_t list -> Node.Attrs.t -> Scope.t -> sexp_t
type parser_resume_t = unit -> State.t

exception ParseFailure of Lexing.position * string * parser_resume_t
(* (position, message, resume continuation) *)
exception ParseSuccess of parser_resume_t

(* Note: the continuation passing style is needed for resumptions and
   to ensure that there are no unnecessary references to previous
   parser states, so that the garbage collector may reclaim the old
   states as soon as possible. *)

let (++) (r1 : parser_rule_t) (r2 : parser_rule_t) =
  fun () (state : State.t) (cont : parser_cont_t) ->
    r1 () state (fun state2 -> r2 () state2 cont)

let (|||) (r1 : parser_rule_t) (r2 : parser_rule_t) =
  fun () (state : State.t) (cont : parser_cont_t) ->
    let success_cont_ref = ref (fun x -> raise (ParseSuccess(fun () -> cont x)))
    in
    let success_cont = (fun x -> !success_cont_ref x)
    in
    try
      r1 () state success_cont
    with
    | ParseFailure(_, _, _) ->
        r2 () state cont
    | TokenStream.Eof ->
        r2 () state cont
    | ParseSuccess(resume) ->
        success_cont_ref := cont;
        resume ()

let (>>) (rule : parser_rule_t) (action : parser_action_t) =
  fun () ((lst, attrs, strm, scope) : State.t) (cont : parser_cont_t) ->
    let attrs1 = new_attrs scope strm
    in
    rule () ([], attrs1, strm, scope)
      (fun (lst2, attrs2, strm2, scope2) ->
        cont ((action (List.rev lst2) attrs2 scope2) :: lst, attrs, strm2, scope))
      (* the above closure (hopefully) will refer only to lst, attrs
      and scope, so that strm could be reclaimed by the gc *)

let (+>) = (>>)

(* +! may be used instead of ++ to indicate that the current rule
   succeeded, thus cancelling the surrounding ||| and allowing the gc
   to free the old states; note that the rule may still fail later.

   Example:

   rule1 ++ rule2 +! rule3 ||| rule4

   This has the same effect as

   rule1 ++ rule2 ++ rule3 ||| rule4

   except that after rule2 succeeds there is no longer a possibility
   of backtracking to rule4, i.e. if rule3 fails, then the whole rule
   fails and rule4 is not tried. The cancellation of backtracking
   allows the gc to reclaim the tokens read by rule1 and rule2 (which
   otherwise must be kept in memory in case rule4 should need
   them). Note that if the backtracting is not cancelled via +! then
   _no_ token read by rule3 may be reclaimed by the gc, because rule4
   might need them. This may be a problem if rule3 is recursive. Thus
   +! should be used when it is known that no other alternative may
   succeed at this point and the remaining part of the branch may eat
   up a significant amount of memory. *)

let (+!) (r1 : parser_rule_t) (r2 : parser_rule_t) =
  fun () (state : State.t) cont ->
    r1 () state
      (fun state2 ->
        raise (ParseSuccess(fun () -> r2 () state2 cont)))

(* To make the ocaml compiler happy, recursive rules must be specified
   like this:

   let rec rule () =
      recursive
         begin
            rule1 ++ rule ||| rule2
         end

   In constrast, normal rules should be specified like this:

   let rule = rule1 ++ rule2 ||| rule3
 *)

let recursive (r : parser_rule_t) = r ()

(* Basic actions *)

let collect lst _ _ = Sexp(lst)

let return sexp _ _ _ = sexp

(* Basic parser rules *)

let empty () state cont = cont state

(* token t recognises a given token t; fails if the token is not
   present; on success leaves the state intact, except for consuming
   the token from the stream *)
let token (token : Token.t) =
  fun () ((lst, attrs, strm, scope) : State.t) (cont : parser_cont_t) ->
    let state2 = (lst, attrs, Scope.strm_next scope strm, scope)
    in
    if Token.eq (Scope.strm_token scope strm) token then
      cont state2
    else
      raise (ParseFailure(Scope.strm_position scope strm,
                          "syntax error",
                          (fun () -> cont state2)))

let symbol sym = token (Token.Symbol(sym))
let keyword sym = token (Token.Keyword(sym))

let number () (lst, attrs, strm, scope) cont =
  match Scope.strm_token scope strm with
  | Token.Number(num) -> cont ((Number(num)) :: lst, attrs, Scope.strm_next scope strm, scope)
  | _ -> raise (ParseFailure(Scope.strm_position scope strm,
                             "expected expression",
                             (fun () -> cont ((Number(Big_int.zero_big_int)) :: lst,
                                              attrs, Scope.strm_next scope strm, scope))))

let lparen = token Token.LeftParen
let rparen = token Token.RightParen
let lparen_sqr = token Token.LeftParenSqr
let rparen_sqr = token Token.RightParenSqr
let lparen_curl = token Token.LeftParenCurl
let rparen_curl = token Token.RightParenCurl

(* skips a single token *)
let skip () ((lst, attrs, strm, scope) : State.t) (cont : parser_cont_t) =
  cont (lst, attrs, Scope.strm_next scope strm, scope)

(* skips tokens until one of the tokens in the list is found *)
let skip_until (tokens : Token.t list) =
  fun () ((lst, attrs, strm, scope) : State.t) (cont : parser_cont_t) ->
    let rec loop strm =
      if Scope.is_strm_empty scope strm then
        cont (lst, attrs, strm, scope)
      else
        begin
          let token = Scope.strm_token scope strm
          in
          if List.exists (fun x -> Token.eq x token) tokens then
            cont (lst, attrs, strm, scope)
          else
            loop (Scope.strm_next scope strm)
        end
    in
    loop strm

let eof () ((lst, attrs, strm, scope) as state) cont =
  if Scope.is_strm_empty scope strm then
    cont state
  else
    raise (ParseFailure(Scope.strm_position scope strm,
                        "syntax error",
                        (fun () -> skip_until [Token.Sep] () state cont)))

(* Additional operations on parser rules *)

let maybe r = r ||| empty

let optional r = maybe r >> collect

(* recognise and discard the resulting list *)
let discard r =
  (fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, scope)
      (fun (_, attrs2, strm2, scope2) ->
        cont (lst, attrs2, strm2, scope2)))

let change_attrs f =
  fun () (lst, attrs, strm, scope) cont ->
    cont (lst, f (List.rev lst) attrs, strm, scope)

let change_scope f =
  fun () (lst, attrs, strm, scope) cont ->
    cont (lst, attrs, strm, f (List.rev lst) scope)

let save_scope (r : parser_rule_t) =
  fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, scope)
      (fun (lst2, attrs2, strm2, _) ->
        cont (lst2, attrs2, strm2, scope))

let new_scope (r : parser_rule_t) =
  fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, Scope.push scope)
      (fun (lst2, attrs2, strm2, _) ->
        cont (lst2, attrs2, strm2, scope))

let new_keyword sym (r : parser_rule_t) =
  fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, Scope.add_keyword scope sym)
      (fun (lst2, attrs2, strm2, _) ->
        cont (lst2, attrs2, strm2, scope))

let new_frame (r : parser_rule_t) =
  fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, Scope.push_frame scope)
      (fun (lst2, attrs2, strm2, _) ->
        cont (lst2, attrs2, strm2, scope))

(* execution of parser rules *)

let execute r keywords builtins symtab lexbufs scope0 =
  let scope =
    List.fold_left
      (fun scope f -> f scope)
      (List.fold_left (fun scope x -> Scope.add_permanent_keyword scope x) scope0 keywords)
      builtins
  in
  let rec scan lst =
    match lst with
    | h :: t -> Scanner.scan_prepend symtab h (fun () -> scan t)
    | [] -> TokenStream.empty
  in
  r () ([], None, scan lexbufs, scope) (fun x -> x)

(* -------------------------------------------------------------------------- *)

let do_parse is_repl_mode lexbufs eval_handler decl_handler =

  let symtab = Symtab.create ()
  in

  (* symbols begin *)

  let sym_dot = Symtab.find symtab "."
  and sym_colon = Symtab.find symtab ":"
  and sym_eq = Symtab.find symtab "="
  and sym_in = Symtab.find symtab "in"
  (*and sym_lazy = Symtab.find symtab "&" *)
  and sym_at = Symtab.find symtab "@"
  and sym_ret_type = Symtab.find symtab ":>"

  and sym_fun = Symtab.find symtab "fun"
  and sym_def = Symtab.find symtab "def"

(*  and sym_syntax = Symtab.find symtab "syntax"
  and sym_drop = Symtab.find symtab "drop"
  and sym_operator = Symtab.find symtab "operator"
  and sym_left = Symtab.find symtab "left"
  and sym_right = Symtab.find symtab "right"
  and sym_prio = Symtab.find symtab "prio"
  and sym_before = Symtab.find symtab "before"
  and sym_after = Symtab.find symtab "after"
  and sym_first = Symtab.find symtab "first"
  and sym_last = Symtab.find symtab "last" *)

  and sym_return_type = Symtab.find symtab "return_type"

  and sym_unknown = Symtab.find symtab "??"

  (* symbols end *)

  in
  let decl f () (lst, attrs, strm, scope) (cont : parser_cont_t) =
    begin
      let pos = Scope.strm_position scope strm
      and error_resume state2 cont =
        skip_until [Token.Symbol(sym_eq); Token.Sep; Token.Symbol(sym_colon)] () state2 cont
      in
      match Scope.strm_token scope strm with
      | Token.Symbol(sym) ->
          let scope2 =
            try
              Some (Scope.add_ident scope sym (f sym pos scope))
            with Scope.Duplicate_ident -> None
          in
          begin
            match scope2 with
            | Some(sc) ->
                cont ((Ident sym) :: lst, attrs, Scope.strm_next sc strm, sc)
            | None ->
                let node = Scope.find_ident scope sym
                in
                let mpos = Node.get_pos node
                in
                let scope3 = Scope.replace_ident scope sym (Node.Proxy(ref Node.Nil))
                in
                let msg =
                  "duplicate identifier `" ^ Symbol.to_string sym ^
                  match mpos with
                  | Some(_) -> "', previous declaration at " ^ Error.pos_to_string mpos
                  | None -> "'"
                in
                Error.error (Some pos) msg;
                error_resume ((Ident sym) :: lst, attrs, Scope.strm_next scope strm, scope3) cont
          end
      | _ ->
          let scope2 = Scope.replace_ident scope sym_unknown (Node.Proxy(ref Node.Nil))
          in
          Error.error (Some pos) "expected identifier";
          error_resume ((Ident sym_unknown) :: lst, attrs, strm, scope2) cont
    end

  and name () (lst, attrs, strm, scope) cont =
    match Scope.strm_token scope strm with
    | Token.Symbol(sym) -> cont ((Ident(sym)) :: lst, attrs, Scope.strm_next scope strm, scope)
    | _ -> raise (ParseFailure(Scope.strm_position scope strm,
                               "expected identifier",
                               (fun () ->
                                 cont ((Ident(sym_unknown)) :: lst, attrs, Scope.strm_next scope strm, scope))))

  and get_singleton_node lst =
    match lst with
    | [Program(node)] -> node
    | _ -> assert false

  and number = number +>
    (fun lst _ _ ->
      match lst with
      | [Number(num)] -> Program(Node.Integer(num))
      | _ -> assert false)

  and repl_eval () ((lst, attrs, _, scope) as state) cont =
    if is_repl_mode && Scope.nesting scope = 0 then
      begin
        match lst with
        | Program(stmt) :: _ ->
            eval_handler stmt (Scope.lineno scope);
            cont state
        | _ -> assert false
      end
    else
      cont state

  and repl_decl () ((lst, attrs, _, scope) as state) cont =
    if is_repl_mode && Scope.nesting scope = 0 then
      begin
        match lst with
        | [Program(value); Ident(_); Bool(is_eager)] ->
            if Node.is_immediate value then
              eval_handler value (Scope.lineno scope)
            else
              decl_handler (if is_eager then value else Node.Delay(value)) (Scope.lineno scope);
            cont state
        | _ -> assert false
      end
    else
      cont state
  in

  (* grammar begin *)

  let rec program () =
    recursive
      begin
        progn ++ eof
      end

  and progn () =
    recursive
      begin
        (if is_repl_mode then repl_statements else statements) ||| empty
          >>
        (fun lst attrs scope ->
          match lst with
          | [x] -> x
          | [] -> Program(Node.Nil)
          | _ ->
              Program(Node.Progn(List.map (function Program(x) -> x | _ -> assert false) lst, attrs)))
      end

  and statements () =
    recursive
      begin
        (token Token.Sep +> return (Program(Node.Nil)) |||
           statement ++ maybe (token Token.Sep)) +!
        maybe statements
      end

  and repl_statements () =
    recursive
      begin
        xlet |||
        begin
          (token Token.Sep +> return (Program(Node.Nil)) |||
             statement ++ maybe (token Token.Sep)) +!
          repl_eval ++
          maybe repl_statements
        end
      end

  and statement () =
    recursive
      begin
        xlet ||| (* xfun ||| def ||| syntax ||| *) expr
      end

(*
<xfun> ::= fun <ident> <args> [>: <term>] [= <expr>]
<def> ::= def <ident> [<args>] [>: <term>] [= <expr>]
<args> ::= <ident> [<args>]
*)

(*
<syntax> ::= syntax (<operator> | <drop>)
<operator> ::= operator <symbol> [is] <oper-spec-lst>
<oper-spec-lst> ::= <oper-spec> [, <oper-spec-lst>]
<oper-spec> ::= left | right | binary | unary | prio <symbol>
             | [prio] after <symbol> | [prio] before <symbol>
             | [prio] last | [prio] first
<drop> ::= drop <symbol-lst>
<symbol-lst> ::= <symbol> [, <symbol-lst>]
*)

  and xlet () =
    recursive
      begin
        (token Token.LetEager +> return (Bool true) ||| token Token.LetLazy +> return (Bool false)) +!
          ident_let ++
          symbol sym_eq ++
          new_keyword sym_in (expr) ++
          (change_scope
             (fun lst scope ->
               match lst with
               | [_; Ident(sym); Program(value)] ->
                   begin
                     match Scope.find_ident scope sym with
                     | Node.Proxy(r) ->
                         let value2 =
                           match value with
                           | Node.Lambda(body, frm, seen, attrs) ->
                               Node.Lambda(body, frm, seen, Node.Attrs.set_name attrs sym)
                           | Node.LambdaEager(body, frm, seen, attrs) ->
                               Node.LambdaEager(body, frm, seen, Node.Attrs.set_name attrs sym)
                           | _ -> value
                         in
                         r := value2;
                         let node =
                           if Node.is_immediate value2 then
                             value2
                           else
                             Node.Var(Scope.frame scope + 1)
                         in
                         let scope2 = Scope.replace_ident scope sym node
                         in
                         if Node.is_immediate value then
                           scope2
                         else
                           Scope.push_frame scope2
                     | _ -> Debug.print (sexp_list_to_string lst); assert false
                   end
               | _ -> assert false)) ++
          (symbol sym_in +! new_scope expr |||
           token Token.Sep ++ repl_decl ++ progn)
          >>
        (fun lst attrs scope ->
          match lst with
          | [Bool(is_eager); Ident(sym); Program(value); Program(body)] ->
              if Node.is_immediate value then
                Program(body)
              else
                Program(Node.Appl(Node.Lambda(body, Scope.frame scope, ref 0, attrs),
                                  (if is_eager then Node.Force(value) else value), None))
          | _ -> assert false)
      end

  and expr () =
    recursive
      begin
        terms
          >>
        (fun lst attrs _ ->
          let rec build_appl lst attrs =
            match lst with
            | [Program(x)] -> x
            | Program(x) :: t -> Node.Appl(build_appl t None, x, attrs)
            | _ -> assert false
          in
          Program(build_appl (List.rev lst) attrs))
      end

  and terms () =
    recursive
      begin
        term ++ maybe terms
      end

  and term () =
    recursive
      begin
        lambda ||| cond |||
        lparen +! new_scope (progn ++ rparen) |||
        lparen_curl +! new_scope (progn ++ rparen_curl) |||
        token Token.Lazy +! term +> (fun lst _ _ -> Program(Node.Delay(get_singleton_node lst))) |||
        token Token.Force +! term +> (fun lst _ _ -> Program(Node.Force(get_singleton_node lst))) |||
        ident_ref ||| number
      end

  and lambda () =
    recursive
      begin
        discard (maybe attributes) ++ token Token.Lambda +!
          (token Token.Force +> return (Bool true) |||
             token Token.Lazy +> return (Bool false) |||
             empty +> return (Bool true)) ++
          new_scope
          (new_frame
             (ident_lambda ++ discard (maybe ret_atype) ++
                (symbol sym_dot +! expr ||| term)))
          >>
        (fun lst attrs scope ->
          match lst with
          | [Bool(is_eager); Ident(sym); Program(body)] ->
              if is_eager then
                Program(Node.LambdaEager(body, Scope.frame scope + 1, ref 0, attrs))
              else
                Program(Node.Lambda(body, Scope.frame scope + 1, ref 0, attrs))
          | _ -> assert false)
      end

  and cond () =
    recursive
      begin
        token Token.If +! expr ++ token Token.Then ++ expr ++ token Token.Else ++ expr
          >>
        (fun lst attrs _ ->
          match lst with
          | [Program(x); Program(y); Program(z)] ->
              Program(Node.Cond(x, y, z, attrs))
          | _ -> assert false)
      end

  and ident_ref =
    name
      >>
    (fun lst attrs scope ->
      match lst with
      | [Ident(sym)] ->
          begin
            try
              Program(Scope.find_ident scope sym)
            with Not_found ->
              Error.error (Node.Attrs.get_pos attrs) "Unbound identifier";
              Program(Node.Nil)
          end
      | _ -> assert false)

  and ident f =
    discard (maybe attributes) ++ decl f ++ discard (maybe atype)

  and ident_let () =
    recursive
      begin
        ident
          (fun sym pos _ ->
            (Node.Proxy(ref (Node.Error(Node.Nil, Node.Attrs.create (Some sym) (Some pos))))))
      end

  and ident_lambda () =
    recursive
      begin
        ident (fun _ _ scope -> Node.Var(Scope.frame scope))
      end

  and atype () =
    recursive
      begin
        (symbol sym_colon) +! term ++
          change_attrs
          (fun lst attrs ->
            Node.Attrs.set_type attrs (get_singleton_node lst))
      end

  and ret_atype () =
    recursive
      begin
        (symbol sym_ret_type) +! term ++
          change_attrs
          (fun lst attrs ->
            Node.Attrs.set_attr attrs sym_return_type (get_singleton_node lst))
      end

  and attributes () =
    recursive
      begin
        attribute ++ maybe attributes
      end

  and attribute () =
    recursive
      begin
        symbol sym_at +! name ++ optional (symbol sym_eq ++ term) ++
          change_attrs
          (fun lst attrs ->
            match lst with
            | [Ident(aname); Sexp([])] ->
                Node.Attrs.set_attr attrs aname Node.Nil
            | [Ident(aname); Sexp([Program(value)])] ->
                Node.Attrs.set_attr attrs aname value
            | _ -> assert false)
      end
  in

  (* grammar end *)

  let rec loop f =
    try
      f ()
    with
    | ParseSuccess(resume) ->
        loop resume
    | ParseFailure(pos, msg, resume) ->
        Error.error (Some(pos)) msg;
        loop resume
    | TokenStream.Eof ->
        Error.fatal "unexpected end of file";
        ([Program(Node.Nil)], None, TokenStream.empty, Scope.empty)
  and kwds = [sym_fun; sym_def]
  and builtins =
    [(fun x -> Generic_builtins.declare_builtins x symtab);
     (fun x -> Arith_builtins.declare_builtins x symtab);
     (fun x -> Bool_builtins.declare_builtins x symtab)]
  and scope0 = if is_repl_mode then Scope.empty_repl else Scope.empty
  in
  let (lst, _, _, _) =
    loop
      (fun () -> execute program kwds builtins symtab lexbufs scope0)
  in
  get_singleton_node lst

let parse lexbufs = do_parse false lexbufs (fun _ _ -> ()) (fun _ _ -> ())
let parse_repl = do_parse true
