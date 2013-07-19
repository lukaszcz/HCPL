
let power x y =
  let rec aux x y acc =
    if y = 0 then
      acc
    else
      aux x (y - 1) (x * acc)
  in
  aux x y 1

let mysqrt x =
  let rec aux x top bot =
    if top - bot <= 1 then
      bot
    else
      begin
        let y = (top + bot) / 2
        in
        if y * y > x then
          aux x y bot
        else
          aux x top y
      end
  in
  aux x (x + 1) 0

let log x y =
  let rec aux x y acc =
    if y = 1 then
      acc
    else
      aux x (y / x) (acc + 1)
  in
  aux x y 0

let fast_power x y =
  let rec aux x y acc =
    if y = 0 then
      acc
    else if y mod 2 = 1 then
      aux (x * x) (y / 2) (x * acc)
    else
      aux (x * x) (y / 2) acc
  in
  aux x y 1

let rec loop x n =
  if n = 0 then
    x
  else if n mod 2 = 0 then
    loop (power x 4) (n - 1)
  else
    loop (mysqrt (mysqrt x)) (n - 1)

let rec loop2 x n =
  if n = 0 then
    x
  else if n mod 2 = 0 then
    loop2 (fast_power 3 x) (n - 1)
  else
    loop2 (log 3 x) (n - 1);;

print_endline (string_of_int (loop 13 1000000));
print_endline (string_of_int (loop (power 13 4) 1000001));

print_endline (string_of_int (fast_power 3 17));
print_endline (string_of_int (loop2 17 1000000));
print_endline (string_of_int (loop2 (power 3 17) 1000001));
