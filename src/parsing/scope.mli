(* scope.mli: Scope interface. A scope object contains information
   associated with the current lexical scope.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t

exception Duplicate_ident
exception Circular_dependency of Symbol.t list

val empty : t
val empty_repl : t
(* push introduces new scope; identifiers added via add_ident will
   override their definitions in previous scopes; non-permanent
   keywords are erased *)
val push : t -> t
(* the same as above, but does not erase non-permanent keywords *)
val push_ident_scope : t -> t
(* return current scope nesting *)
val nesting : t -> int

(* raises Duplicate_ident if symbol already present in the most recent
   scope *)
val add_ident : t -> Symbol.t -> Node.t -> t
val import_ident : t -> Symbol.t -> Node.t -> t
(* raises Not_found if not found; find_ident automatically translates
   Var(idx) to Var(frame - idx) (de Bruijn form) *)
val find_ident : t -> Symbol.t -> Node.t
val replace_ident : t -> Symbol.t -> Node.t -> t
val drop_ident : t -> Symbol.t -> t
(* returns the indentifier table for the current scope only *)
val identtab : t -> Node.t Symbol.Map.t

val add_fwd_decl : t -> Symbol.t -> int (* id *) -> t
val is_fwd_decl : t -> Symbol.t -> bool
val get_fwd_decl : t -> Symbol.t -> int (* id *) * int (* frame *) * Node.t ref
val remove_fwd_decl : t -> Symbol.t -> t

(* enter_module changes current module; raises Circular_dependency if
   the module has already been entered *)
val enter_module : t -> Symbol.t -> t
val leave_module : t -> t
val current_module : t -> Symbol.t
val is_module_mode : t -> bool

val enter_match : t -> t
val add_placeholder : t -> Symbol.t -> t
val placeholders : t -> Symbol.t list
val is_match_mode : t -> bool

val add_keyword : t -> Symbol.t -> t
val add_permanent_keyword : t -> Symbol.t -> t

(* this is not the line number -- this is an ugly hack for repl
   display to work OK *)
val lineno : t -> int

(* return the current token, translating symbols into keywords in
   accordance with the keyword table of the current scope *)
val strm_token : t -> TokenStream.t -> Token.t
val strm_position : t -> TokenStream.t -> Lexing.position
val strm_next : t -> TokenStream.t -> TokenStream.t
val is_strm_empty : t -> TokenStream.t -> bool

(* returns the current frame number *)
val frame : t -> int
(* increases the current frame number *)
val push_frame : t -> t
(* decreases the current frame number *)
val pop_frame : t -> t

val rewrite : t -> Node.t list -> Node.t
val add_oper : t -> Symbol.t -> Opertab.prio_t -> int (* assoc *) -> int (* arity *) -> t
val drop_oper : t -> Symbol.t -> t

val add_block : t -> Symbol.t (* begin symbol *) -> Symbol.t (* end symbol *) -> t
(* returns the block end symbol for a given block start symbol; raises
   Not_found if the argument is not a valid block start symbol *)
val get_block_end : t -> Symbol.t -> Symbol.t
val is_block_end : t -> Symbol.t -> bool

val add_macrosep : t -> Symbol.t -> t
val is_macrosep : t -> Symbol.t -> bool

val get_syntax : t -> Syntax.t list
val add_syntax : t -> Syntax.t list -> t
