(* parser.ml: Parser implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

(* -------------------------------------------------------------------------- *)
(* Parser rules implementation *)

(* The parser rules return lists of nodes (plus actually a state, but
   this is an implementation detail). The parser rules may be built up
   from basic parser rules using ++, ^|| and >>.

   rule1 ++ rule2

   means: return the concatenation of the results of rule1 and rule2
   retaining changes to scope (table of identifiers) made inside rule1
   and rule2

   rule1 ^|| rule2

   means: try performing rule1; if successful, return the result of
   rule1; if rule1 fails then return the result of rule2

   rule >> action

   means: save the current scope; evaluate rule; then perform action
   on the result of rule, returning a singleton list containing the
   result of the action; finally, restore the previous scope,
   i.e. forget all changes to scope (table of identifiers) made inside
   rule

   Note that the priority of the operators from most tightly binding
   to least tightly binding is: ++, ^||, >>. Hence e.g.

   rule1 ++ rule2 ^|| rule3 >> action

   is equivalent to

   ((rule1 ++ rule2) ^|| rule3) >> action

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
  | String of string
  | Sexp of sexp_t list
  | Assoc of int
  | Arity of int
  | Prio of Opertab.prio_t

(* pretty printing *)
let rec sexp_to_string sexp =
  match sexp with
  | Program(node) -> "Program(" ^ Node.to_string node ^ ")"
  | Ident(sym) -> "Ident(" ^ Symbol.to_string sym ^ ")"
  | Bool(b) -> "Bool(" ^ (if b then "true" else "false") ^ ")"
  | Number(num) -> "Number(" ^ Big_int.string_of_big_int num ^ ")"
  | String(str) -> "String(\"" ^ str ^ "\")"
  | Sexp(lst) -> "Sexp(" ^ sexp_list_to_string lst ^ ")"
  | Assoc(x) -> "Assoc(" ^ string_of_int x ^ ")"
  | Arity(x) -> "Arity(" ^ string_of_int x ^ ")"
  | Prio(_) -> "Prio"
and sexp_list_to_string lst = Utils.list_to_string sexp_to_string lst

let create_attrs scope strm =
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

let (^||) (r1 : parser_rule_t) (r2 : parser_rule_t) =
  fun () (state : State.t) (cont : parser_cont_t) ->
    let success_cont_ref = ref (fun x -> raise (ParseSuccess(fun () -> cont x)))
    in
    let success_cont = (fun x -> !success_cont_ref x)
    in
    try
      r1 () state success_cont
    with
    | ParseFailure(_) | TokenStream.Eof ->
        r2 () state cont
    | ParseSuccess(resume) ->
        success_cont_ref := cont;
        resume ()

let (>>) (rule : parser_rule_t) (action : parser_action_t) =
  fun () ((lst, attrs, strm, scope) : State.t) (cont : parser_cont_t) ->
    let attrs1 = create_attrs scope strm
    in
    rule () ([], attrs1, strm, scope)
      (fun (lst2, attrs2, strm2, scope2) ->
        cont ((action (List.rev lst2) attrs2 scope2) :: lst, attrs, strm2, scope2))
      (* the above closure (hopefully) will refer only to lst and
      attrs, so that strm could be reclaimed by the gc *)

let (+>) = (>>)

(* +! may be used instead of ++ to indicate that the current rule
   succeeded, thus cancelling the surrounding ^|| and allowing the gc
   to free the old states; note that the rule may still fail later.

   Example:

   rule1 ++ rule2 +! rule3 ^|| rule4

   This has the same effect as

   rule1 ++ rule2 ++ rule3 ^|| rule4

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
            rule1 ++ rule ^|| rule2
         end

   In constrast, normal rules should be specified like this:

   let rule = rule1 ++ rule2 ^|| rule3
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

let peek (token : Token.t) =
  fun () ((lst, attrs, strm, scope) as state) (cont : parser_cont_t) ->
    if Token.eq (Scope.strm_token scope strm) token then
      cont state
    else
      raise (ParseFailure(Scope.strm_position scope strm,
                          "syntax error",
                          (fun () -> cont state)))

let symbol sym = token (Token.Symbol(sym))
let keyword sym = token (Token.Keyword(sym))

let number () (lst, attrs, strm, scope) cont =
  match Scope.strm_token scope strm with
  | Token.Number(num) -> cont (Number(num) :: lst, attrs, Scope.strm_next scope strm, scope)
  | _ -> raise (ParseFailure(Scope.strm_position scope strm,
                             "expected a number",
                             (fun () -> cont ((Number(Big_int.zero_big_int)) :: lst,
                                              attrs, Scope.strm_next scope strm, scope))))

let string () (lst, attrs, strm, scope) cont =
  match Scope.strm_token scope strm with
  | Token.String(str) -> cont (String(str) :: lst, attrs, Scope.strm_next scope strm, scope)
  | _ -> raise (ParseFailure(Scope.strm_position scope strm,
                             "expected a string",
                             (fun () -> cont (String("") :: lst,
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

let maybe r = r ^|| empty

let optional r = maybe r >> collect

let fail msg lst0 =
  (fun () ((lst, attrs, strm, scope) as state) cont ->
    raise (ParseFailure(Scope.strm_position scope strm, msg,
                        (fun () ->
                          skip () state
                            (fun (lst, attrs, strm, scope) ->
                              cont (lst0 @ lst, attrs, strm, scope))))))

(* recognise and discard the resulting list *)
let discard r =
  (fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, scope)
      (fun (_, attrs2, strm2, scope2) ->
        cont (lst, attrs2, strm2, scope2)))

let rule (r : parser_rule_t) =
  (fun () (lst, attrs, strm, scope) cont ->
    r () ([], create_attrs scope strm, strm, scope)
      (fun (lst2, _, strm2, scope2) ->
        cont (lst2 @ lst, attrs, strm2, scope2)))

let catch_errors r =
  (fun () (lst, attrs, strm, scope) cont ->
    try
      r () (lst, attrs, strm, scope)
        (fun (lst2, attrs2, strm2, scope2) ->
          cont (lst2, attrs2, strm2, scope2))
    with
      ParseFailure(pos, msg, resume) ->
        Error.error (Some pos) msg;
        resume ())

let change_attrs f =
  fun () (lst, attrs, strm, scope) cont ->
    cont (lst, f (List.rev lst) attrs, strm, scope)

let change_scope f =
  fun () (lst, attrs, strm, scope) cont ->
    cont (lst, attrs, strm, f (List.rev lst) attrs scope)

let new_scope (r : parser_rule_t) =
  fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, Scope.push scope)
      (fun (lst2, attrs2, strm2, _) ->
        cont (lst2, attrs2, strm2, scope))

let save_scope (r : parser_rule_t) =
  fun () (lst, attrs, strm, scope) cont ->
    r () (lst, attrs, strm, scope)
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

let execute r lexbuf symtab scope =
  let strm =
    Scanner.scan_prepend symtab lexbuf (fun () -> TokenStream.empty)
  in
  r () ([], None, strm, scope) (fun x -> x)

(* -------------------------------------------------------------------------- *)

let do_parse is_repl_mode lexbuf runtime_lexbuf eval_handler decl_handler =

  let symtab = Symtab.create ()
  in

  (* symbols begin *)

  let sym_dot = Symtab.find symtab "."
  and sym_colon = Symtab.find symtab ":"
  and sym_comma = Symtab.find symtab ","
  and sym_eq = Symtab.find symtab "="
  and sym_in = Symtab.find symtab "in"
  and sym_is = Symtab.find symtab "is"
  and sym_at = Symtab.find symtab "@"
  and sym_ret_type = Symtab.find symtab ":>"

  and sym_fun = Symtab.find symtab "fun"
  and sym_def = Symtab.find symtab "def"

  and sym_syntax = Symtab.find symtab "syntax"
  and sym_drop = Symtab.find symtab "drop"
  and sym_operator = Symtab.find symtab "operator"
  and sym_left = Symtab.find symtab "left"
  and sym_right = Symtab.find symtab "right"
  and sym_prio = Symtab.find symtab "prio"
  and sym_before = Symtab.find symtab "before"
  and sym_after = Symtab.find symtab "after"
  and sym_first = Symtab.find symtab "first"
  and sym_last = Symtab.find symtab "last"
  and sym_binary = Symtab.find symtab "binary"
  and sym_unary = Symtab.find symtab "unary"
  and sym_appl = Symtab.find symtab "appl"

  and sym_import = Symtab.find symtab "import"
  and sym_open = Symtab.find symtab "open"
  and sym_include = Symtab.find symtab "include"
  and sym_module = Symtab.find symtab "module"

  and sym_return_type = Symtab.find symtab "return_type"

  and sym_load_module = Symtab.find symtab "__ipl_load_module"
  and sym_record_get = Symtab.find symtab "__ipl_record_get"

  and sym_unknown = Symtab.find symtab "??"

  (* symbols end *)

  in

  let rec mkprogn lst attrs =
    match lst with
    | [] -> Node.Nil
    | _ ->
        Node.Progn(List.map (function Program(x) -> x | _ -> assert false) lst, attrs)

  and mkapply scope sym lst =
    let node = Scope.find_ident scope sym
    in
    List.fold_left (fun acc x -> Node.Appl(acc, x, None)) node lst

  and mkmodule scope sym node frm =
    Node.Delayed(ref (mkapply scope sym_load_module
                        [Node.Sym(sym); node; Node.Integer(Big_int.big_int_of_int frm)]))

  and join_syms sym1 sym2 =
    Symtab.find symtab (Symbol.to_string sym1 ^ "." ^ Symbol.to_string sym2)

  and unique_module_id =
    let id = ref 0
    in
    fun () -> incr id; Symtab.find symtab ("__ipl__module__" ^ string_of_int !id)
  in

  let rec do_parse_lexbuf is_repl_mode lexbuf initial_scope =

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

    and comma = symbol sym_comma

    and get_singleton_node lst =
      match lst with
      | [Program(node)] -> node
      | _ -> assert false

    and number = number +>
      (fun lst _ _ ->
        match lst with
        | [Number(num)] -> Program(Node.Integer(num))
        | _ -> assert false)

    and string = string +>
      (fun lst _ _ ->
        match lst with
        | [String(str)] -> Program(Node.String(str))
        | _ -> assert false)

    and repl_eval () ((lst, attrs, _, scope) as state) cont =
      if is_repl_mode && Scope.nesting scope = 0 then
        begin
          match lst with
          | Program(stmt) :: _ ->
              eval_handler stmt (Scope.lineno scope);
              cont state
          | [] ->
              eval_handler Node.Nil (Scope.lineno scope);
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

    and load_module f =
      (fun () (lst, attrs, strm, scope) cont ->
        match lst with
        | Ident(sym) :: _ ->
            begin
              let empty_module_tuple =
                (Symbol.Map.empty, [], Node.Record(Symbol.Map.empty))
              in
              let (identtab, lst2, node) =
                try
                  let node = Scope.find_ident scope sym
                  in
                  match node with
                  | Node.Record(identtab) -> (identtab, [], node)
                  | _ ->
                      Error.error (Node.Attrs.get_pos attrs) "expected a constant module";
                      empty_module_tuple
                with Not_found ->
                  try
                    let (identtab, node) =
                      Loader.load_module (Symbol.to_string sym)
                        (fun lexbuf ->
                          let (node, scope) =
                            do_parse_lexbuf false lexbuf (Scope.enter_module (Scope.push initial_scope) sym)
                          in
                          (Scope.identtab scope, node))
                    in
                    let module_node = mkmodule scope sym node (Scope.frame initial_scope)
                    in
                    if Node.is_module_closed node then
                      (identtab, [Program(module_node)], Node.Record(identtab))
                    else
                      (identtab, [Program(module_node)], module_node)
                  with
                  | Not_found ->
                      Error.error (Node.Attrs.get_pos attrs) "module not found";
                      empty_module_tuple
                  | Scope.Circular_dependency(lst) ->
                      Error.error (Node.Attrs.get_pos attrs)
                        ("circular module dependencies: " ^ Utils.list_to_string Symbol.to_string lst);
                      empty_module_tuple
              in
              let scope2 = f sym node identtab attrs scope
              in
              cont (lst2, attrs, strm, scope2)
            end
        | _ -> assert false)

    and add_idents f =
      (fun sym module_node identtab attrs scope ->
        Symbol.Map.fold
          (fun k node scope ->
            let node2 =
              match module_node with
              | Node.Record(_) -> node
              | _ -> mkapply scope sym_record_get [module_node; Node.Sym(k)]
            in
            try
              Scope.add_ident scope (f sym k) node2
            with Scope.Duplicate_ident ->
              Error.error (Node.Attrs.get_pos attrs) ("duplicate identifier: " ^ (Symbol.to_string (f sym k)));
              scope
          )
          identtab
          scope
      )
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
          (if is_repl_mode then repl_statements else statements) ^|| empty
            >>
          (fun lst attrs _ -> Program(mkprogn lst attrs))
        end

    and statements () =
      recursive
        begin
          (token Token.Sep +> return (Program(Node.Nil)) ^||
          statement ++ maybe (token Token.Sep)) +!
            maybe statements
        end

    and repl_statements () =
      recursive
        begin
          xlet ^||
          begin
            (token Token.Sep +> return (Program(Node.Nil)) ^||
            statement ++ maybe (token Token.Sep)) +!
              repl_eval ++
              maybe repl_statements
          end
        end

    and statement () =
      recursive
        begin
          xlet ^|| syntax ^|| import ^|| xopen ^|| xinclude ^|| xmodule ^|| module_end ^|| expr
        end

    and xlet () =
      recursive
        begin
          (token Token.LetEager +> return (Bool true) ^|| token Token.LetLazy +> return (Bool false)) +!
            ident_let ++
            symbol sym_eq ++
            new_keyword sym_in (catch_errors expr) ++
            (change_scope
               (fun lst _ scope ->
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
                           if Node.is_immediate value2 then
                             scope2
                           else
                             Scope.push_frame scope2
                       | _ -> Debug.print (sexp_list_to_string lst); assert false
                     end
                 | _ -> assert false)) ++
            (symbol sym_in +! new_scope expr ++
               (change_scope
                  (fun lst _ scope ->
                    match lst with
                    | [_; Ident(sym); _; _] -> Scope.drop_ident scope sym
                    | _ -> assert false))
           ^||
             token Token.Sep ++ repl_decl ++ progn
           ^||
             (peek Token.RightParenCurl ^|| token Token.Eof)
               +>
             (fun _ _ scope ->
               if Scope.is_module_mode scope then
                 Program(Node.MakeRecord(Scope.identtab scope))
               else
                 Program(Node.Nil))) ++
            (change_scope
               (fun lst _ scope ->
                 match lst with
                 | [_; _; Program(value); _] ->
                     if Node.is_immediate value then
                       scope
                     else
                       Scope.pop_frame scope
                 | _ -> assert false))
            >>
          (fun lst attrs scope ->
            match lst with
            | [Bool(is_eager); Ident(sym); Program(value); Program(body)] ->
                if Node.is_immediate value then
                  Program(body)
                else
                  Program(Node.Appl(Node.Lambda(body, Scope.frame scope + 1, ref 0, attrs),
                                    (if is_eager then Node.Force(value) else value), None))
            | _ -> assert false)
        end

    and syntax () =
      recursive
        begin
          symbol sym_syntax +! catch_errors (operator ^|| drop)
        end

    and operator () =
      recursive
        begin
          rule
            (discard
               (symbol sym_operator +! name ++ maybe (symbol sym_is) ++ oper_spec_list ++
                  (change_scope
                     (fun lst attrs scope ->
                       let prio_lst = List.filter (fun x -> match x with Prio(_) -> true | _ -> false) lst
                       and assoc_lst = List.filter (fun x -> match x with Assoc(_) -> true | _ -> false) lst
                       and arity_lst = List.filter (fun x -> match x with Arity(_) -> true | _ -> false) lst
                       and sym = match List.hd lst with Ident(sym) -> sym | _ -> assert false
                       in
                       let prio =
                         match prio_lst with
                         | Prio(x) :: _ -> x
                         | _ -> Opertab.EqualAppl
                       and assoc =
                         match assoc_lst with
                         | Assoc(x) :: _ -> x
                         | _ -> 1
                       and arity =
                         match arity_lst with
                         | Arity(x) :: _ -> x
                         | _ -> 1
                       in
                       if List.length prio_lst > 1 then
                         Error.error (Node.Attrs.get_pos attrs) "multiple operator priority specifications";
                       if List.length assoc_lst > 1 then
                         Error.error (Node.Attrs.get_pos attrs) "multiple operator associativity specifications";
                       if List.length arity_lst > 1 then
                         Error.error (Node.Attrs.get_pos attrs) "multiple operator arity specifications";
                       try
                         Scope.add_oper scope sym prio assoc arity
                       with Not_found ->
                         Error.error (Node.Attrs.get_pos attrs) "undeclared operator";
                         scope))))
        end

    and oper_spec_list () =
      recursive
        begin
          oper_spec ++ maybe (comma +! oper_spec_list)
        end

    and oper_spec =
      symbol sym_left +> return (Assoc Opertab.assoc_left) ^||
      symbol sym_right +> return (Assoc Opertab.assoc_right) ^||
      symbol sym_binary +> return (Arity 2) ^||
      symbol sym_unary +> return (Arity 1) ^||
      maybe (symbol sym_prio) ++ symbol sym_after ++ name
        +>
      (fun lst attrs scope ->
        match lst with
        | [Ident(sym)] ->
            if Symbol.eq sym sym_appl then
              Prio(Opertab.AfterAppl)
            else
              Prio(Opertab.After(sym))
        | _ -> assert false)
    ^||
      maybe (symbol sym_prio) ++ symbol sym_before ++ name
        +>
      (fun lst attrs scope ->
        match lst with
        | [Ident(sym)] ->
            if Symbol.eq sym sym_appl then
              Prio(Opertab.BeforeAppl)
            else
              Prio(Opertab.Before(sym))
        | _ -> assert false)
    ^||
      maybe (symbol sym_prio) ++ symbol sym_last
        +>
      return (Prio(Opertab.Last))
    ^||
      maybe (symbol sym_prio) ++ symbol sym_first
        +>
      return (Prio(Opertab.First))
    ^||
      symbol sym_prio ++ name
        +>
      (fun lst attrs scope ->
        match lst with
        | [Ident(sym)] ->
            if Symbol.eq sym sym_appl then
              Prio(Opertab.EqualAppl)
            else
              Prio(Opertab.Equal(sym))
        | _ -> assert false)

    and drop () =
      recursive
        begin
          rule
            (symbol sym_drop +! name_list ++
               (change_scope
                  (fun lst attrs scope ->
                    List.fold_left
                      (fun scope x ->
                        match x with
                        | Ident(sym) ->
                            begin
                              try
                                Scope.drop_oper scope sym
                              with Not_found ->
                                Error.error (Node.Attrs.get_pos attrs) "undeclared operator";
                                scope
                            end
                        | _ -> assert false)
                      scope
                      lst)))
        end

    and name_list () =
      recursive
        begin
          name ++ maybe (comma +! name_list)
        end

    and import =
      rule (symbol sym_import +! catch_errors name ++ (load_module (add_idents join_syms)))

    and xopen =
      rule (symbol sym_open +! catch_errors name ++ (load_module (add_idents (fun _ k -> k))))

    and xinclude =
      rule
        begin
          symbol sym_include +! catch_errors string ++
            (fun () (lst, attrs, strm, scope) cont ->
              match lst with
              | [Program(Node.String(str))] ->
                  begin
                    let maybe_lexbuf =
                      try
                        let lexbuf = Lexing.from_channel (open_in str)
                        in
                        lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p
                                                    with
                                                      Lexing.pos_fname = str
                                                    };
                        Some(lexbuf)
                      with Sys_error(msg) ->
                        Error.error (Node.Attrs.get_pos attrs) msg;
                        None
                    in
                    match maybe_lexbuf with
                    | Some(lexbuf) ->
                        cont ([], None, Scanner.scan_prepend symtab lexbuf (fun () -> strm), scope)
                    | None ->
                        cont ([], None, strm, scope)
                  end
              | _ -> assert false)
        end

    and xmodule () =
      recursive
        begin
          rule
            begin
              symbol sym_module +! catch_errors name ++ catch_errors (token Token.LeftParenCurl) ++
                (fun () (lst, attrs, strm, scope) cont ->
                  match lst with
                  | [Ident(sym)] ->
                      let module_name = join_syms (Scope.current_module scope) sym
                      in
                      progn () ([], create_attrs scope strm, strm,
                                Scope.enter_module (Scope.push scope) (unique_module_id ()))
                        (fun (lst2, attrs2, strm2, scope2) ->
                          let node = get_singleton_node lst2
                          in
                          let m =
                            mkmodule scope2 (unique_module_id ()) node (Scope.frame scope2)
                          in
                          let identtab = Scope.identtab scope2
                          in
                          let m2 =
                            if Node.is_module_closed node then
                              Node.Record(identtab)
                            else
                              m
                          in
                          let scope3 =
                            try
                              let scope4 = Scope.add_ident (Scope.add_ident scope module_name m2) sym m2
                              in
                              add_idents join_syms sym m2 identtab attrs scope4
                            with Scope.Duplicate_ident ->
                              Error.error (Node.Attrs.get_pos attrs) "duplicate identifier";
                              scope
                          in
                          cont ([Program(m)], create_attrs scope3 strm2, strm2, scope3))
                  | _ -> Debug.print (sexp_list_to_string lst); assert false
                ) ++
                catch_errors (token Token.RightParenCurl)
            end
        end

    and module_end =
      (token Token.Eof
         >>
       (fun _ _ scope ->
         if Scope.is_module_mode scope then
           Program(Node.MakeRecord(Scope.identtab scope))
         else
           Program(Node.Nil)))
    ^||
      (peek Token.RightParenCurl ++
         (fun () (lst, attrs, strm, scope) cont ->
           if Scope.is_module_mode scope then
             let sexp = Program(Node.MakeRecord(Scope.identtab scope))
             in
             cont (sexp :: lst, attrs, strm,
                   Scope.leave_module scope)
               (* This is a bit of a hack. We need to set module_mode
               to false (by calling leave_module) so that this rule
               does not succeed for the second time. The scope is then
               discarded anyway, because the only way we can see a '}'
               in module mode (on correct input) is with the `module X
               { ... }' construction *)
           else
             raise (ParseFailure(TokenStream.position strm,
                                 "internal error: module failure",
                                 (fun () -> assert false)))))

    and expr () =
      recursive
        begin
          terms
            >>
          (fun lst attrs scope ->
            let node = Scope.rewrite scope (List.map (function Program(x) -> x | _ -> assert false) lst)
            in
            match node with
            | Node.Appl(x, y, _) -> Program(Node.Appl(x, y, attrs))
            | _ -> Program(node))
        end

    and terms () =
      recursive
        begin
          term ++ maybe terms
        end

    and term () =
      recursive
        begin
          lambda ^|| cond ^||
          lparen +! new_scope (catch_errors (progn ++ rparen)) ^||
          lparen_curl +! new_scope (catch_errors (progn ++ rparen_curl)) ^||
          token Token.Lazy +! term +> (fun lst _ _ -> Program(Node.Delay(get_singleton_node lst))) ^||
          token Token.Force +! term +> (fun lst _ _ -> Program(Node.Force(get_singleton_node lst))) ^||
          token Token.True +> return (Program(Node.True)) ^||
          token Token.False +> return (Program(Node.False)) ^||
          ident_ref ^|| number ^|| string ^||
          fail "expected expression" [Program(Node.Nil)]
        end

    and lambda () =
      recursive
        begin
          discard (maybe attributes) ++ token Token.Lambda +!
            (catch_errors
               (token Token.Force +> return (Bool true) ^||
               token Token.Lazy +> return (Bool false) ^||
               empty +> return (Bool true)) ++
               new_scope
               (new_frame
                  (ident_lambda ++ discard (maybe ret_atype) ++
                     (symbol sym_dot +! expr ^|| term))))
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
          token Token.If +! catch_errors expr ++ token Token.Then ++
            catch_errors expr ++ token Token.Else ++ catch_errors expr
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
              (Node.Proxy(ref Node.Nil)))
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
    in
    let (lst, _, _, scope) =
      loop
        (fun () -> execute program lexbuf symtab initial_scope)
    in
    (get_singleton_node lst, scope)
  in
  let keywords = [sym_fun; sym_def]
  and builtins =
    [(fun x -> Generic_builtins.declare_builtins x symtab);
     (fun x -> Arith_builtins.declare_builtins x symtab);
     (fun x -> Bool_builtins.declare_builtins x symtab);
     (fun x -> String_builtins.declare_builtins x symtab);
     (fun x -> Record_builtins.declare_builtins x symtab)]
  in
  let scope0 =
    List.fold_left
      (fun scope f -> f scope)
      (List.fold_left
         (fun scope x -> Scope.add_permanent_keyword scope x)
         (if is_repl_mode then Scope.empty_repl else Scope.empty)
         keywords)
      builtins
  in
  match runtime_lexbuf with
  | Some(buf) ->
      let scope1 = snd (do_parse_lexbuf false buf scope0)
      in
      let (node, scope) = do_parse_lexbuf is_repl_mode lexbuf scope1
      in
      (Scope.identtab scope, node)
  | None ->
      let (node, scope) = do_parse_lexbuf is_repl_mode lexbuf scope0
      in
      (Scope.identtab scope, node)

let parse lexbuf runtime_lexbuf = do_parse false lexbuf runtime_lexbuf (fun _ _ -> ()) (fun _ _ -> ())
let parse_repl = do_parse true
