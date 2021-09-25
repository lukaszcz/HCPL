(* bignum.ml: Big numbers encoded in nodes -- implementation.

   Copyright (C) 2013 by Łukasz Czajka
*)

open Node
open Big_int

let from_int v =
  if is_smallint_value v then
    make_smallint v
  else
    Integer(big_int_of_int v)

let from_big_int v =
  if is_int_big_int v then
    begin
      let x = int_of_big_int v
      in
      if is_smallint_value x then
        make_smallint x
      else
        Integer(v)
    end
  else
    Integer(v)

let to_int x =
  if is_smallint x then
    smallint_value x
  else
    match x with
    | Integer(a) -> int_of_big_int a
    | _ -> Error.runtime_error ("not a number: " ^ Node.to_string x)

let to_big_int x =
  if is_smallint x then
    big_int_of_int (smallint_value x)
  else
    match x with
    | Integer(a) -> a
    | _ -> Error.runtime_error ("not a number: " ^ Node.to_string x)

let is_number x = is_smallint x || (match x with Integer(_) -> true | _ -> false)

(* NOTE: depends on OCaml implementation *)
let msb x =
  let rec loop x bit =
    if x land (1 lsl bit) = 0 then
      loop x (bit - 1)
    else
      bit + 1
  in
  if x = 0 then 0 else loop (abs x) Config.int_bits

let mul_fits x y =
  msb x + msb y <= smallint_bits

(* "rec" to prevent inlining *)
let [@warning "-39"]rec do_gt x y =
  if is_smallint x then
    begin
      assert (not (is_smallint y));
      match y with
      | Integer(b) -> if sign_big_int b < 0 then True else False
      | _ -> BGt(x, y)
    end
  else
    match x with
    | Integer(a) ->
        if is_smallint y then
          begin
            if sign_big_int a > 0 then True else False
          end
        else
          begin
            match y with
            | Integer(b) -> if gt_big_int a b then True else False
            | _ -> BGt(x, y)
          end
    | _ -> BGt(x, y)

let gt x y =
  if is_smallint x && is_smallint y then
    begin
      if ((Obj.magic x) : int) > ((Obj.magic y) : int) then True else False
    end
  else
    do_gt x y

let [@warning "-39"]rec do_ge x y =
  if is_smallint x then
    begin
      assert (not (is_smallint y));
      match y with
      | Integer(b) -> if sign_big_int b < 0 then True else False
      | _ -> BGe(x, y)
    end
  else
    match x with
    | Integer(a) ->
        if is_smallint y then
          begin
            if sign_big_int a > 0 then True else False
          end
        else
          begin
            match y with
            | Integer(b) -> if ge_big_int a b then True else False
            | _ -> BGe(x, y)
          end
    | _ -> BGe(x, y)

let ge x y =
  if is_smallint x && is_smallint y then
    begin
      if ((Obj.magic x) : int) >= ((Obj.magic y) : int) then True else False
    end
  else
    do_ge x y

let [@warning "-39"]rec do_add x y =
  if is_smallint x then
    begin
      assert (not (is_smallint y));
      match y with
      | Integer(b) -> from_big_int (add_int_big_int (smallint_value x) b)
      | _ -> BAdd(x, y)
    end
  else
    match x with
    | Integer(a) ->
        if is_smallint y then
          from_big_int (add_int_big_int (smallint_value y) a)
        else
          begin
            match y with
            | Integer(b) -> from_big_int (add_big_int a b)
            | _ -> BAdd(x, y)
          end
    | _ -> BAdd(x, y)

let add x y =
  if is_smallint x && is_smallint y then
    from_int (smallint_value x + smallint_value y)
  else
    do_add x y

let [@warning "-39"]rec do_sub x y =
  if is_smallint x then
    begin
      assert (not (is_smallint y));
      match y with
      | Integer(b) -> from_big_int (add_int_big_int (smallint_value x) (minus_big_int b))
      | _ -> BSub(x, y)
    end
  else
    match x with
    | Integer(a) ->
        if is_smallint y then
          from_big_int (add_int_big_int (-(smallint_value y)) a)
        else
          begin
            match y with
            | Integer(b) -> from_big_int (sub_big_int a b)
            | _ -> BSub(x, y)
          end
    | _ -> BSub(x, y)

let sub x y =
  if is_smallint x && is_smallint y then
    from_int (smallint_value x - smallint_value y)
  else
    do_sub x y

let [@warning "-39"]rec do_mul x y =
  if is_smallint x then
    begin
      assert (not (is_smallint y));
        match y with
        | Integer(b) -> from_big_int (mult_int_big_int (smallint_value x) b)
        | _ -> BMul(x, y)
    end
  else
    match x with
    | Integer(a) ->
        if is_smallint y then
          from_big_int (mult_int_big_int (smallint_value y) a)
        else
          begin
            match y with
            | Integer(b) -> from_big_int (mult_big_int a b)
            | _ -> BMul(x, y)
          end
    | _ -> BMul(x, y)

let mul x y =
  if is_smallint x && is_smallint y then
    begin
      let a = smallint_value x
      and b = smallint_value y
      in
      if mul_fits a b then
        from_int (a * b)
      else
        from_big_int (mult_int_big_int a (big_int_of_int b))
    end
  else
    do_mul x y

let [@warning "-39"]rec do_idiv x y =
  if is_smallint x then
    begin
      assert (not (is_smallint y));
      match y with
      | Integer(b) -> from_big_int (div_big_int (big_int_of_int (smallint_value x)) b)
      | _ -> BIDiv(x, y)
    end
  else
    match x with
    | Integer(a) ->
        if is_smallint y then
          from_big_int (div_big_int a (big_int_of_int (smallint_value y)))
        else
          begin
            match y with
            | Integer(b) -> from_big_int (div_big_int a b)
            | _ -> BIDiv(x, y)
          end
    | _ -> BIDiv(x, y)

let idiv x y =
  if is_smallint x && is_smallint y then
    from_int (smallint_value x / smallint_value y)
  else
    do_idiv x y

let [@warning "-39"]rec do_modulo x y =
  if is_smallint x then
    begin
      assert (not (is_smallint y));
      match y with
      | Integer(b) -> from_big_int (mod_big_int (big_int_of_int (smallint_value x)) b)
      | _ -> BMod(x, y)
    end
  else
    match x with
    | Integer(a) ->
        if is_smallint y then
          from_big_int (mod_big_int a (big_int_of_int (smallint_value y)))
        else
          begin
            match y with
            | Integer(b) -> from_big_int (mod_big_int a b)
            | _ -> BMod(x, y)
          end
    | _ -> BMod(x, y)

let modulo x y =
  if is_smallint x && is_smallint y then
    from_int (smallint_value x mod smallint_value y)
  else
    do_modulo x y
