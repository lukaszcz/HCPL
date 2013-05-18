(* symtab.ml: Symbol table implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

module H =
  struct
    type t = Symbol.t
    let equal x y = (Symbol.to_string x = Symbol.to_string y)
    let hash = Symbol.hash
  end

module MyHash = Weak.Make(H)

type t = MyHash.t

let create () = MyHash.create 512

let find symtab s =
  let sym = Symbol.alloc s
  in
  MyHash.merge symtab sym
