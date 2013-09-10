(* core_builtins.mli: Core builtins implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node
open Big_int

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
  | x :: init_node :: y :: env ->
      begin
        match x, y with
        | bfrm, Sym(sym) ->
            begin
              try
                Symbol.Hash.find module_hash sym
              with Not_found ->
                let frm = Bignum.to_int bfrm
                in
                assert (frm <= Env.length env);
                let env2 = Env.pop_n env (Env.length env - frm - 1)
                in
                let node = Eval.eval_in init_node env2
                in
                Symbol.Hash.add module_hash sym node;
                node
            end
        | _ -> assert false
      end
  | _ -> Debug.print (Utils.list_to_string Node.to_string lst); assert false

(* quoting *)

let ipl_quote lst =
  match lst with
  | x :: _ ->
      Quote.quote x
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
  | _ -> Error.runtime_error "eval: bad argument"

let reduce lst =
  match lst with
  | Quoted(x) :: _ -> Node.mkquoted (Eval.reduce x)
  | _ -> Error.runtime_error "reduce: bad argument"

let eval_limited lst =
  match lst with
  | y :: Quoted(x) :: _ -> Node.mkquoted (Eval.eval_limited x (Bignum.to_int y))
  | _ -> Error.runtime_error "eval-limited: bad arguments"

(* occurs check *)

let occurs_check lst =
  match lst with
  | y :: x :: _ -> if Quote.occurs_check x y then True else False
  | _ -> Error.runtime_error "occurs-check: bad arguments"

(* operations on tokens *)

let join_tokens lst =
  match lst with
  | Tokens(lst2) :: Tokens(lst1) :: _ -> Tokens(lst1 @ lst2)
  | _ -> Error.runtime_error "join-tokens: bad arguments"

let split_tokens lst =
  match lst with
  | Tokens(toks) :: _ -> List.fold_right (fun x acc -> Cons(Tokens([x]), acc)) toks Nil
  | _ -> Error.runtime_error "split-tokens: bad argument"

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

let unique_int_token =
  let id = ref 0
  in
  fun lst ->
    match lst with
    | Nil :: _ -> incr id; Node.Tokens([(Token.Number(Big_int.big_int_of_int !id), Lexing.dummy_pos)])
    | _ -> Error.runtime_error "unique-int-token: expected '()' as the sole argument"

(* public interface *)

let declare_builtins scope symtab =
  begin
    Random.self_init ();

    (* set inline builtins names *)

    Node.change_name Node.eq (Symtab.find symtab "=");
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
    Node.change_name Node.xnot (Symtab.find symtab "not");
    Node.change_name Node.xand (Symtab.find symtab "and");
    Node.change_name Node.xor (Symtab.find symtab "or");

    (* declare inline builtins *)

    let scope = Scope.add_ident scope (Symtab.find symtab "=") Node.eq in
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
    let scope = Scope.add_ident scope (Symtab.find symtab "not") Node.xnot in
    let scope = Scope.add_ident scope (Symtab.find symtab "and") Node.xand in
    let scope = Scope.add_ident scope (Symtab.find symtab "or") Node.xor in

    (* declare other builtins *)

    let (scope, _) = Builtin.declare scope (Symtab.find symtab "print") (prn, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "exit") (myexit, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_load_module") (load_module, 3, CallByName) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "quote") (ipl_quote, 1, CallByName) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "'") (ipl_quote, 1, CallByName) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "random") (xrandom, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "^") (concat, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "to_string") (to_string, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "eval") (eval, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "reduce") (reduce, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "eval-limited") (eval_limited, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "occurs-check") (occurs_check, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "join-tokens") (join_tokens, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "split-tokens") (split_tokens, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "error") (runtime_error, 1, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "__ipl_macro_tmp") (macro_tmp, 2, CallByValue) in
    let (scope, _) = Builtin.declare scope (Symtab.find symtab "unique-int-token") (unique_int_token, 1, CallByValue) in

    let scope =
      Scope.add_ident scope (Symtab.find symtab "token-tokens-start")
        (Node.Tokens([(Token.TokensStart, Lexing.dummy_pos)])) in

    let scope =
      Scope.add_ident scope (Symtab.find symtab "token-tokens-end")
        (Node.Tokens([(Token.TokensEnd, Lexing.dummy_pos)])) in

    scope
  end
