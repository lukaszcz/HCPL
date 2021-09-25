(* Note: This is a naive translation of test_004.ipl. It is actually
   slower, because the HCPL interpreter is better at keeping track of
   what really needs a bignum and what doesn't. The HCPL interpreter
   uses unboxed integers whenever possible, but converts automatically
   to big integers when necessary.  *)

open Big_int

let power x y =
  let rec aux x y acc =
    if y = 0 then
      acc
    else
      aux x (y - 1) (mult_int_big_int x acc)
  in
  aux x y unit_big_int

let mysqrt x =
  let rec aux x top bot =
    if le_big_int (sub_big_int top bot) unit_big_int then
      bot
    else
      begin
        let y = div_big_int (add_big_int top bot) (big_int_of_int 2)
        in
        if gt_big_int (mult_big_int y y) x then
          aux x y bot
        else
          aux x top y
      end
  in
  aux x (add_int_big_int 1 x) zero_big_int

let log x y =
  let rec aux x y acc =
    if eq_big_int y unit_big_int then
      acc
    else
      aux x (div_big_int y x) (acc + 1)
  in
  aux (big_int_of_int x) y 0

let fast_power x y =
  let rec aux x y acc =
    if y = 0 then
      acc
    else if y mod 2 = 1 then
      aux (mult_big_int x x) (y / 2) (mult_big_int x acc)
    else
      aux (mult_big_int x x) (y / 2) acc
  in
  aux (big_int_of_int x) y unit_big_int

let rec loop x n =
  if n = 0 then
    int_of_big_int x
  else if n mod 2 = 0 then
    loop (power (int_of_big_int x) 4) (n - 1)
  else
    loop (mysqrt (mysqrt x)) (n - 1)

let rec loop2 x n =
  if n = 0 then
    (int_of_big_int x)
  else if n mod 2 = 0 then
    loop2 (fast_power 7 (int_of_big_int x)) (n - 1)
  else
    loop2 (big_int_of_int (log 7 x)) (n - 1);;

print_endline (string_of_int (loop (big_int_of_int 10000) 100000));
print_endline (string_of_int (loop (power 10000 4) 100001));

print_endline (string_of_int (loop2 (big_int_of_int 10000) 100));
print_endline (string_of_int (loop2 (power 7 10000) 101));

print_endline (string_of_int (log 5 (power 5 100000)));
print_endline (string_of_int (log 7 (power 7 100000)));
