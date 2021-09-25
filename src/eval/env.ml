(* env.ml: Evaluation environment implementation.

Copyright (C) 2013 by Łukasz Czajka

*)

type t = Node.t list

let empty = [Node.Dynenv(Utils.IntMap.empty)]

let rec do_nth env n =
  assert (n >= 0);
  match env with
  | h :: t ->
      begin
        assert (env != []);
        if n > 0 then
          do_nth t (n - 1)
        else
          h
      end
  | [] -> assert (env <> []); Node.Nil

let nth env n =
  if n = 0 then
    match env with
    | x :: _ -> x
    | [] -> assert (env <> []); Node.Nil
  else if n = 1 then
    match env with
    | _ :: x :: _ -> x
    | _ -> assert (1 = 0); Node.Nil
  else
    match env with
    | _ :: _ :: t -> do_nth t (n - 2)
    | _ -> assert (1 = 0); Node.Nil

let push env x = x :: env

let rec pop_n env n =
  assert (n >= 0);
  if n > 0 then
    begin
      assert (env != []);
      match env with
      | _ :: t -> pop_n t (n - 1)
      | [] -> assert (env <> []); []
    end
  else
    env

let rec push_n env x n =
  if n = 0 then
    env
  else
    push_n (x :: env) x (n - 1)

let length env = List.length env
