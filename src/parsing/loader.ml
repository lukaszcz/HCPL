(* loader.ml: Module loader implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

type identtab_t = Node.t Symbol.Map.t

let load_module name parse =
    assert (name <> "");
    let name2 = String.copy name
    in
    name2.[0] <- Char.lowercase (name2.[0]);
    let rec loop lst =
      match lst with
      | h :: t ->
          begin
            let path = h ^ Config.dir_sep () ^ name2 ^ ".ipl"
            in
            try
              let lexbuf = Lexing.from_channel (open_in path)
              in
              lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p
                                          with
                                            Lexing.pos_fname =
                                            if h = "." then
                                              path
                                            else
                                              String.sub path 2 (String.length path - 2)
                                          };
              parse lexbuf
            with Sys_error(_) ->
              loop t
          end
      | [] -> raise Not_found
    in
    loop (Config.path ())
