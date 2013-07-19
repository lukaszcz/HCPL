
type 'a strm_t = Cons of 'a * ('a strm_t lazy_t)

let rec filter strm p =
  match strm with
  | Cons(x, t) -> if p x then Cons(x, lazy (filter (Lazy.force t) p)) else filter (Lazy.force t) p

let rec take strm n = if n = 0 then [] else match strm with Cons(h, t) -> h :: (take (Lazy.force t) (n - 1))

let rec nth strm n = match strm with Cons(x, t) -> if n = 1 then x else nth (Lazy.force t) (n - 1)

let rec numbers n = Cons(n, lazy (numbers (n + 1)))

let divisible n x = x mod n <> 0

let hd (Cons(x, _)) = x
let tl (Cons(_, t)) = Lazy.force t

let rec eratostenes strm =
  let n = hd strm
  in
  Cons(n, lazy (eratostenes (filter (tl strm) (divisible n))))

let primes = eratostenes (numbers 2)

let greater n x = x > n

let f n lst =
  let rec aux m lst =
    if m = n then lst else aux (m + 1) (filter lst (greater m))
  in
  aux 0 lst;;

let list_to_string f lst =
  let rec prn lst =
    match lst with
    | [x] -> f x
    | h :: t -> (f h) ^ ", " ^ prn t
    | [] -> ""
  in
  "[" ^ prn lst ^ "]"

let print_list lst = print_endline (list_to_string (fun n -> string_of_int n) lst);;

print_endline (string_of_int (hd (f 3000 (numbers 0))));
print_endline (string_of_int (hd (filter (numbers 0) (fun x -> x > 10000000))));

print_list (take primes 10);
print_endline (string_of_int (nth primes 100));
print_endline (string_of_int (nth primes 1000));
print_endline (string_of_int (nth primes 1000));
print_endline (string_of_int (nth primes 5793));
print_endline (string_of_int (nth primes 5793));
print_endline (string_of_int (nth primes 10000));
print_endline (string_of_int (nth primes 10000))
