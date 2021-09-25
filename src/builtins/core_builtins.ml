(* core_builtins.ml: Core builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node

let is_number lst =
  match lst with
  | x :: _ ->
      if Node.is_immediate x then
        begin
          if Bignum.is_number x then True else False
        end
      else
        x
  | _ -> assert false

let is_string lst =
  match lst with
  | x :: _ ->
      if Node.is_immediate x then
        begin
          match x with String(_) -> True | _ -> False
        end
      else
        x
  | _ -> assert false

let prn lst =
  match lst with
  | String(str) :: _ -> print_endline str; Nil
  | h :: _ -> print_endline (Node.to_string h); Nil
  | _ -> assert false

let myexit lst =
  match lst with
  | Nil :: _ -> exit 0
  | num :: _ -> exit (Bignum.to_int num)
  | _ -> assert false

(* modules *)

let module_hash = Symbol.Hash.create 16

let load_module lst =
  match lst with
  | x :: init_node :: y :: _ ->
      begin
        match x, y with
        | _, Sym(sym) ->
            begin
              try
                Symbol.Hash.find module_hash sym
              with Not_found ->
                (*let frm = Bignum.to_int bfrm
                in
                assert (frm < Env.length env);
                let env2 = Env.pop_n env (Env.length env - frm - 1)
                in*) (* TODO: this does not fully work *)
                (*Debug.print "load_module";
                Debug.print (Node.to_string init_node);*)
                let node = Eval.eval init_node
                in
                Symbol.Hash.add module_hash sym node;
                node
            end
        | _ -> assert false
      end
  | _ -> Debug.print (Utils.list_to_string Node.to_string lst); assert false

(* matching *)

let xmatch lst =
  match lst with
  | u :: z :: y :: x :: _ ->
      begin
        let node =
          try
            Appl(z, Node.list_to_cons (List.rev (Match.xmatch x y [])), None)
          with
            Exit -> Appl(u, Nil, None)
        in
        Eval.eval node
      end
  | _ -> assert false

(* quoting etc *)

let mark lst =
  match lst with
  | y :: Quoted(x) :: _ ->
      Node.mkquoted (Marked(x, y))
  | y :: x :: _ when Node.is_quoted x ->
      Node.mkquoted (Marked(x, y))
  | _ ->
      Error.runtime_error "mark: wrong arguments"

let subst lst =
  match lst with
  | z :: y :: x :: _ -> Quote.subst x y z
  | _ -> assert false

let lift lst =
  match lst with
  | y :: x :: _ -> Quote.lift x y
  | _ -> assert false

let lift_marked lst =
  match lst with
  | y :: x :: _ -> Quote.lift_marked x y
  | _ -> assert false

let close lst =
  match lst with
  | x :: _ -> Quote.close x
  | _ -> assert false

let reduce_eta lst =
  match lst with
  | x :: _ -> Quote.eta_reduce x
  | _ -> assert false

(* random *)

let xrandom lst =
  match lst with
  | x :: _ -> Bignum.from_int (Random.int (Bignum.to_int x))
  | _ -> Error.runtime_error "random: type error"

(* strings *)

let concat lst =
  match lst with
  | String(b) :: String(a) :: _ -> String(a ^ b)
  | _ -> Error.runtime_error "concat: type error"

let to_string lst =
  match lst with
  | x :: _ -> String(Node.to_string x)
  | _ -> assert false

(* evaluation *)

let eval lst =
  match lst with
  | Quoted(x) :: _ -> Node.mkquoted (Eval.eval x)
  | x :: _ when Node.is_quoted x -> x
  | x :: _ -> Error.runtime_error ("eval: bad argument: " ^ Node.to_string x)
  | _ -> assert false

let reduce lst =
  match lst with
  | Quoted(x) :: _ -> Node.mkquoted (Eval.reduce x)
  | x :: _ when Node.is_quoted x -> x
  | x :: _ -> Error.runtime_error ("reduce: bad argument: " ^ Node.to_string x)
  | _ -> assert false

let apply lst =
  match lst with
  | Quoted(y) :: Quoted(x) :: _ -> Node.mkquoted (Eval.apply x y)
  | Quoted(y) :: x :: _ when Node.is_quoted x -> Node.mkquoted (Eval.apply x y)
  | y :: Quoted(x) :: _ when Node.is_quoted y -> Node.mkquoted (Eval.apply x y)
  | y :: x :: _ when Node.is_quoted x && Node.is_quoted y -> Node.mkquoted (Eval.apply x y)
  | _ -> Error.runtime_error ("reduce: bad arguments")

let eval_limited lst =
  match lst with
  | y :: Quoted(x) :: _ -> Node.mkquoted (Eval.eval_limited x (Bignum.to_int y))
  | y :: x :: _ when Node.is_quoted x && Bignum.is_number y -> x
  | y :: x :: _ -> Error.runtime_error ("eval-limited: bad arguments: " ^ Node.to_string x ^ ", " ^ Node.to_string y)
  | _ -> assert false

let eval_unlimited lst =
  match lst with
  | Quoted(x) :: _ -> Node.mkquoted (Eval.eval_unlimited x)
  | x :: _ when Node.is_quoted x -> x
  | x :: _ -> Error.runtime_error ("eval-unlimited: bad argument: " ^ Node.to_string x)
  | _ -> assert false

(* occurs check *)

let occurs_check lst =
  match lst with
  | y :: x :: _ -> if Quote.occurs_check x y then True else False
  | _ -> assert false

(* operations on tokens *)

let join_tokens lst =
  match lst with
  | Tokens(lst2) :: Tokens(lst1) :: _ -> Tokens(lst1 @ lst2)
  | _ -> Error.runtime_error "join-tokens: bad arguments"

let split_tokens lst =
  match lst with
  | Tokens(toks) :: _ -> List.fold_right (fun x acc -> Cons(Tokens([x]), acc)) toks Nil
  | _ -> Error.runtime_error "split-tokens: bad argument"

let tokens_to_string lst =
  match lst with
  | Tokens(toks) :: _ -> String(List.fold_right (fun (t, _) acc -> Token.to_string t ^ acc) toks "")
  | _ -> Error.runtime_error "tokens-to-string: bad argument"

let to_tokens lst =
  match lst with
  | String(s) :: _ -> Tokens([Token.String(s), Lexing.dummy_pos])
  | x :: _ when Bignum.is_number x -> Tokens([Token.Number(Bignum.to_big_int x), Lexing.dummy_pos])
  | _ -> Error.runtime_error "to_tokens: bad argument"

(* runtime errors *)

let runtime_error lst =
  match lst with
  | String(s) :: _ -> Error.runtime_error s
  | _ -> Error.runtime_error "runtime-error: bad arguments"

(* macro support *)

let macro_tmp lst =
  match lst with
  | y :: x :: _ ->
      begin
        let n = Bignum.to_int x
        in
        if n < 0 || n > 9 then
          Error.runtime_error "bad macro temp number"
        else
          match List.nth (List.rev (Eval.extra_macro_args ())) n, y with
          | Tokens([(tok, _)]), Tokens([(_, pos)]) -> Tokens([(tok, pos)])
          | r, _ -> r
      end
  | _ -> failwith "macro_tmp"

let macro_file lst =
  match lst with
  | x :: _ ->
      begin
        let pos2 =
          match x with
          | Tokens([(_, pos)]) -> pos
          | _ -> Lexing.dummy_pos
        in
        match Eval.macro_pos () with
        | Some(pos) ->
            Tokens([(Token.String(pos.Lexing.pos_fname), pos2)])
        | None ->
            Tokens([(Token.String(pos2.Lexing.pos_fname), pos2)])
      end
  | _ -> assert false

let macro_line lst =
  match lst with
  | x :: _ ->
      begin
        let pos2 =
          match x with
          | Tokens([(_, pos)]) -> pos
          | _ -> Lexing.dummy_pos
        in
        match Eval.macro_pos () with
        | Some(pos) ->
            Tokens([(Token.Number(Big_int.big_int_of_int pos.Lexing.pos_lnum), pos2)])
        | None ->
            Tokens([(Token.Number(Big_int.big_int_of_int pos2.Lexing.pos_lnum), pos2)])
      end
  | _ -> assert false

let macro_column lst =
  match lst with
  | x :: _ ->
      begin
        let pos2 =
          match x with
          | Tokens([(_, pos)]) -> pos
          | _ -> Lexing.dummy_pos
        in
        match Eval.macro_pos () with
        | Some(pos) ->
            Tokens([(Token.Number(Big_int.big_int_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)), pos2)])
        | None ->
            Tokens([(Token.Number(Big_int.big_int_of_int (pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol)), pos2)])
      end
  | _ -> assert false

let unique_int =
  let id = ref 0
  in
  fun lst ->
    match lst with
    | Nil :: _ -> incr id; Bignum.from_int !id
    | _ -> Error.runtime_error "unique-int: expected '()' as the sole argument"

let join_symbols lst =
  match lst with
  | Tokens([(Token.Symbol(y), _)]) :: Tokens([(Token.Symbol(x), pos)]) :: _ ->
      let symtab = Eval.macro_symtab ()
      in
      Tokens([(Token.Symbol(Symtab.find symtab (Symbol.to_string x ^ Symbol.to_string y)), pos)])
  | _ -> Error.runtime_error "join-symbols: expected two symbol tokens as arguments"

(* exceptions *)

let xtry lst =
  match lst with
  | y :: x :: _ ->
      begin
        try
          Eval.eval x
        with
          Error.RuntimeError(node) ->
            Eval.eval (Appl(y, node, None))
      end
  | _ -> assert false

let xraise lst =
  match lst with
  | x :: _ ->
      raise (Error.RuntimeError(x))
  | _ -> assert false

(* public interface *)

let declare_builtins scope symtab =
  begin
    Random.self_init ();

    (* set inline builtins names *)

    Node.change_name Node.eq (Symtab.find symtab "==");
    Node.change_name Node.gt (Symtab.find symtab ">");
    Node.change_name Node.lt (Symtab.find symtab "<");
    Node.change_name Node.ge (Symtab.find symtab ">=");
    Node.change_name Node.le (Symtab.find symtab "<=");
    Node.change_name Node.add (Symtab.find symtab "+");
    Node.change_name Node.sub (Symtab.find symtab "-");
    Node.change_name Node.mul (Symtab.find symtab "*");
    Node.change_name Node.idiv (Symtab.find symtab "div");
    Node.change_name Node.xmod (Symtab.find symtab "mod");
    Node.change_name Node.cons (Symtab.find symtab "cons");
    Node.change_name Node.cons_comma (Symtab.find symtab ",");
    Node.change_name Node.cons_dcolon (Symtab.find symtab "::");
    Node.change_name Node.cons_lazy (Symtab.find symtab "cons&");
    Node.change_name Node.xfst (Symtab.find symtab "fst");
    Node.change_name Node.xsnd (Symtab.find symtab "snd");
    Node.change_name Node.xhd (Symtab.find symtab "hd");
    Node.change_name Node.xtl (Symtab.find symtab "tl");
    Node.change_name Node.xand (Symtab.find symtab "and");
    Node.change_name Node.xor (Symtab.find symtab "or");

    (* declare inline builtins *)

    let scope = Scope.add_ident scope (Symtab.find symtab "==") Node.eq in
    let scope = Scope.add_ident scope (Symtab.find symtab ">") Node.gt in
    let scope = Scope.add_ident scope (Symtab.find symtab "<") Node.lt in
    let scope = Scope.add_ident scope (Symtab.find symtab ">=") Node.ge in
    let scope = Scope.add_ident scope (Symtab.find symtab "<=") Node.le in
    let scope = Scope.add_ident scope (Symtab.find symtab "+") Node.add in
    let scope = Scope.add_ident scope (Symtab.find symtab "-") Node.sub in
    let scope = Scope.add_ident scope (Symtab.find symtab "*") Node.mul in
    let scope = Scope.add_ident scope (Symtab.find symtab "div") Node.idiv in
    let scope = Scope.add_ident scope (Symtab.find symtab "mod") Node.xmod in
    let scope = Scope.add_ident scope (Symtab.find symtab "cons") Node.cons in
    let scope = Scope.add_ident scope (Symtab.find symtab "cons&") Node.cons_lazy in
    let scope = Scope.add_ident scope (Symtab.find symtab ",") Node.cons_comma in
    let scope = Scope.add_ident scope (Symtab.find symtab "::") Node.cons_dcolon in
    let scope = Scope.add_ident scope (Symtab.find symtab "fst") Node.xfst in
    let scope = Scope.add_ident scope (Symtab.find symtab "snd") Node.xsnd in
    let scope = Scope.add_ident scope (Symtab.find symtab "hd") Node.xhd in
    let scope = Scope.add_ident scope (Symtab.find symtab "tl") Node.xtl in
    let scope = Scope.add_ident scope (Symtab.find symtab "and") Node.xand in
    let scope = Scope.add_ident scope (Symtab.find symtab "or") Node.xor in

    (* declare other builtins *)

    let (scope, _) = Builtin.declare scope (Symtab.find symtab "is-number") (is_number, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "is-string") (is_string, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "print") (prn, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "exit") (myexit, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__hcpl_load_module") (load_module, 3, CallByName) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "xmatch") (xmatch, 4, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "mark") (mark, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "subst") (subst, 3, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "lift") (lift, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "lift-marked") (lift_marked, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "close") (close, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "reduce-eta") (reduce_eta, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "random") (xrandom, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "^") (concat, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "to_string") (to_string, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "eval") (eval, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "reduce") (reduce, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "apply") (apply, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "eval-limited") (eval_limited, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "eval-unlimited") (eval_unlimited, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "occurs-check") (occurs_check, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "join-tokens") (join_tokens, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "split-tokens") (split_tokens, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "tokens-to-string") (tokens_to_string, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "to_tokens") (to_tokens, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "error") (runtime_error, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__hcpl_macro_tmp") (macro_tmp, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__hcpl_file") (macro_file, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__hcpl_line") (macro_line, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__hcpl_column") (macro_column, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "unique-int") (unique_int, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "join-symbols") (join_symbols, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "try") (xtry, 2, CallByName) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "raise") (xraise, 1, CallByName) in

    let scope =
      Scope.add_ident scope (Symtab.find symtab "token-tokens-start")
        (Node.Tokens([(Token.TokensStart, Lexing.dummy_pos)])) in

    let scope =
      Scope.add_ident scope (Symtab.find symtab "token-tokens-end")
        (Node.Tokens([(Token.TokensEnd, Lexing.dummy_pos)])) in

    scope
  end
