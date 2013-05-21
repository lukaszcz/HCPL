(* scope.mli: Scope interface. A scope object contains information
   associated with the current lexical scope.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t

exception Duplicate_ident

val empty : t
val empty_repl : t
(* push introduces new scope; identifiers added via add_ident will
   override their definitions in previous scopes; non-permanent
   keywords are erased *)
val push : t -> t
(* return current scope nesting *)
val nesting : t -> int

(* raises Duplicate_ident if symbol already present in the most recent
   scope *)
val add_ident : t -> Symbol.t -> Node.t -> t
(* raises Not_found if not found; find_ident automatically translates
   Var(idx) to Var(frame - idx) (de Bruijn form) *)
val find_ident : t -> Symbol.t -> Node.t
val replace_ident : t -> Symbol.t -> Node.t -> t

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
