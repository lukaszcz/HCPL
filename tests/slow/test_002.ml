
let cons x y = x :: y
let tl = List.tl
let hd = List.hd

let rec revappend lst1 lst2 =
  match lst1 with
  | h :: t -> revappend t (h :: lst2)
  | [] -> lst2

let reverse lst =
  let rec aux lst acc = if lst = [] then acc else aux (List.tl lst) ((List.hd lst) :: acc)
  in
  aux lst []

let append lst1 lst2 = revappend (reverse lst1) lst2

let flatten lst =
   let rec aux lst acc =
      match lst with
      | h :: t -> aux t (revappend h acc)
      | [] -> acc
   in
   reverse (aux lst [])

let length lst =
   let rec aux lst acc = if lst = [] then acc else aux (List.tl lst) (acc + 1)
   in
   aux lst 0

let split lst n =
  let rec aux lst n acc =
    if n = 0 or lst = [] then
      (reverse acc, lst)
    else
      aux (List.tl lst) (n - 1) ((List.hd lst) :: acc)
  in
  aux lst n []

let merge lst1 lst2 =
  let rec aux acc lst1 lst2 =
    if lst1 = [] then
      revappend lst2 acc
    else if lst2 = [] then
      revappend lst1 acc
    else if hd lst1 < hd lst2 then
      aux (cons (hd lst1) acc) (tl lst1) lst2
    else
      aux (cons (hd lst2) acc) lst1 (tl lst2)
  in
  reverse (aux [] lst1 lst2)

let rec sort lst =
  let n = length lst
  in
  if n < 2 then
    lst
  else
    let (left, right) = split lst (n / 2)
    in
    merge (sort left) (sort right)

let uniq lst =
  let rec aux lst prev acc =
    if lst = [] then
      reverse acc
    else if hd lst = prev then
      aux (tl lst) prev acc
    else
      aux (tl lst) (hd lst) (cons (hd lst) acc)
  in
  if lst = [] then [] else aux (tl lst) (hd lst) [hd lst]

let rec take n lst = if n = 0 or lst = [] then [] else cons (hd lst) (take (n - 1) (tl lst))

let rec gen n f acc = if n = 0 then acc else gen (n - 1) f (cons (f n) acc)
let rec gen1 m n acc = if n = 0 then acc else gen1 m (n - 1) (cons (gen m (fun x -> (Random.int x)) []) acc)
let rec gen2 m n acc = if n = 0 then acc else gen2 m (n - 1) (cons (gen m (fun x -> n) []) acc)

let list_to_string f lst =
  let rec prn lst =
    match lst with
    | [x] -> f x
    | h :: t -> (f h) ^ ", " ^ prn t
    | [] -> ""
  in
  "[" ^ prn lst ^ "]"

let print_list lst = print_endline (list_to_string (fun n -> string_of_int n) lst);;

Random.self_init ();

print_list (uniq (sort (flatten (gen1 5 10000 []))));
print_list (uniq (sort (flatten (gen1 10 20000 []))));
print_list (uniq (sort (flatten (gen1 5 20000 []))));

print_list (take 10 (uniq (sort (flatten (gen2 6 10000 [])))));
print_list (take 10 (uniq (sort (flatten (gen2 9 20000 [])))));
print_list (take 10 (uniq (sort (flatten (gen2 6 20000 [])))));
