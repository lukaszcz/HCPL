
let rec f x a b =
  if x = 0 then
    a
  else
    f (x - 1) (a + (Lazy.force b)) b

let rec fe x a b =
  if x = 0 then
    a
  else
    fe (x - 1) (a + b) b;;

print_endline (string_of_int (fe 100000 0 4));
print_endline (string_of_int (fe 100000 0 2));

print_endline (string_of_int (f 100000 0 (lazy 4)));
print_endline (string_of_int (f 100000 0 (lazy 2)));

print_endline (string_of_int (fe 10000000 0 4));
print_endline (string_of_int (fe 10000000 0 2));

print_endline (string_of_int (f 10000000 0 (lazy 4)));
print_endline (string_of_int (f 10000000 0 (lazy 2)));
