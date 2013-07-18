(* env.ml: Evaluation environment implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

type t = Node.t list

let empty = [Node.Nil]

let rec nth env n =
  assert (n >= 0);
  match env with
  | h :: t ->
      begin
        assert (env != empty);
        if n > 0 then
          nth t (n - 1)
        else
          h
      end
  | [] -> assert (env <> []); Node.Nil

let push env x = x :: env

let rec pop_n env n =
  assert (n >= 0);
  if n > 0 then
    begin
      assert (env != empty);
      match env with
      | h :: t -> pop_n t (n - 1)
      | [] -> assert (env <> []); []
    end
  else
    env

let length env = List.length env - 1
