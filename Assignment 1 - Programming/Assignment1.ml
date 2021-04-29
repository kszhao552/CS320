(*
Honor code comes here:

First Name: Kradon 
Last Name: Zhao
BU ID: U04815381

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)


(* 
a print_list function useful for debugging.
*)

let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l

  in let _ = print_string "[" 
  in let _ = aux ls
  in         print_string "]" 


(* 
reverse: takes a list and reverses the order of it, utilizes tail recursion so there will be no extra 
calls in the call stack
*)
let reverse (ls: 'a list): 'a list = 
  let rec aux ls accum = 
    match ls with
      [] -> accum
    |
      h::t -> aux t (h::accum)
  in aux ls []


(* Problems *)

(*
TODO: Write a function called between that lists the integers between two integers (inclusive)
If the first number is greater then the second return the empty list
the solution should be tail recursive

For example,
between 4 7 = [4; 5; 6; 7]
between 3 3 = [3]
between 10 2 = []
between 4 1000000 does not stack overflow
*)


let rec between (n:int) (e:int): int list = 
  let rec aux (n:int) (e:int) (accum:int list):int list = 
    let i = n in
    if i > e then accum
    else (aux (n+1) e (i::accum))
  in reverse(aux n e [])



(*
TODO: Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, combine as long as possible
your method should be tail recursive.

For example,
zip_int [1;2;3;5] [6;7;8;9] = [(1,6);(2,7);(3,8);(5,9)]
zip_int [1] [2;4;6;8] = [(1,2)]
zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)


let zip_int (a: int list) (b: int list): (int * int) list = 
  let rec aux  a b accum = 
    match a, b with 
      [],[] -> accum
    |
      _::_, [] -> accum
    |
      [], _::_ -> accum
    |
      h1::t1, h2::t2-> aux t1 t2 ((h1, h2)::accum)

  in reverse (aux a b [])


(*
TODO: Write a dotProduct function for lists of integers,
If the two list are of unequal lengths then return 0

For example,
dotProduct [1;2;3;4] [6;7;8;9] = 80            (since 1*6+2*7+3*8+4*9 = 80)
dotProduct [1;2;3;4] [6] = 0
*)

let rec dotProduct (x: int list) (y: int list): int = 
  let rec aux x y accum =
    match x, y with
      [], [] -> accum
    |
      h1::t1, h2::t2 -> aux t1 t2 (h1*h2 + accum)
    |
      _::_, [] -> 0
    |
      [], _::_ -> 0
  in aux x y 0


(* 
TODO:
Write a function that takes a list of tuples and returns a string representation of that list

your representation should be valid as OCaml source:
* every element of a list must be separated by ";"
* the list must be wrapped in "[" and "]"
* tuples should (1,2)
* You may use whitespace however you like

For example,
list_of_tuple_as_string [(1,2);(3,4);(5,6)] = "[ (1,2); (3,4); (5,6) ]"
*)


let rec list_of_tuple_as_string (ls: (int*int) list): string = 
  let rec aux ls = match ls with 
      []->  " "
    |
      (h1, h2)::[] -> "("^string_of_int(h1)^","^string_of_int(h2)^")"
    |
      (h1, h2)::t -> 
      "("^string_of_int(h1)^","^string_of_int(h2)^");"^aux t
  in "["^(aux ls)^"]"

(*let rec aux ls = match ls with
  | [] -> print_string ""
  | e::[] -> print_int e
  | e::l -> 
    let _ = print_int e 
    in let _ = print_string "; " 
    in aux l

  in let _ = print_string "[" 
  in let _ = aux ls
  in         print_string "]" *)


(* 
TODO:
Write an insertion sort function for lists of integers

for example,
sort [6;7;1] = [1;6;7]
*)

(* 
Hint: We encourage you to write the following helper function 

let rec insert (i: int) (list: int list): int list = failwith "unimplemented"

that takes a a number, an already sorted ls and returns a new sorted list with that number inserted
for example,
insert 5 [1;3;5;7] = [1;3;5;5;7]

You can  then call this helper function inside sort. 
*)
let rec insert (i:int) (l: int list): int list = 
  match l with
    [] -> i::l
  |
    h::t-> if (i<=h) then i::h::t
    else h:: (insert i t)


let rec sort (ls: int list): int list = 
  let rec aux (ls: int list) (accum: int list): int list = 
    match ls with
      [] -> accum
    |
      h::t -> aux t (insert h accum)
  in aux ls []


let _ = 
  print_list(between 1 5);
  print_newline ();
  print_int(dotProduct [1;2;3;4] [6;7;8;9]);
  print_newline ();
  print_int(dotProduct [1;2;3;4] [1]);
  print_newline ();
  print_list (sort [6;7;1]);
  print_newline ();
  print_string(list_of_tuple_as_string (zip_int [1;2;3;5] [6;7;8;9]));
  print_newline ();
  print_string(list_of_tuple_as_string ([(0,0)]));
  print_newline ();

