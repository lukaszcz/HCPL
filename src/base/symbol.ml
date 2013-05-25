(* symbol.ml: Symbol type and related functions.

   Copyright (C) 2013 by Łukasz Czajka
*)

type t = { s_str : string; s_hash : int; s_id : int }
type symbol_t = t

let eq = (==)
let to_string x = x.s_str
let hash x = x.s_hash

(* Note: alloc should not be called directly; use Symtab.find instead. *)
let alloc =
  (* WARNING: this will crash if there are ever more than 2^31 (2^63)
  _calls_ to Symtab.find *)
  let id = ref 0
  in
  fun str ->
    incr id;
    if !id = 0 then failwith "Symbol.alloc crashed";
    { s_str = str; s_hash = Hashtbl.hash str; s_id = !id }

module OrderedType =
  struct
    type t = symbol_t
    let compare x y = compare x.s_id y.s_id
  end

module HashedType =
  struct
    type t = symbol_t
    let equal = eq
    let hash = hash
  end

module Hash = Hashtbl.Make(HashedType)

module Map = Map.Make(OrderedType)

module Set = Set.Make(OrderedType)
