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


let rec print_int_list (ls:int list) = 
  match ls with
    [] -> print_newline
  |
    h1::t1 -> print_int h1; print_string " ";print_int_list t1

let rec print_int_option (ls: int list option) = 
  match ls with
    None -> print_newline
  |
    Some x -> print_int_list x

let rec print_int_tuple_list (ls:(int*int) list) = 
  match ls with
    [] -> print_newline
  |
    (x, y)::t1 -> print_int x; print_string " ";print_int y; print_string", "; print_int_tuple_list t1

let rec print_int_tuple_option (ls: (int*int) list option) = 
  match ls with
    None -> print_newline
  |
    Some x -> print_int_tuple_list x
(*
Write a safe_zip_int function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, return None
your method must be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow

Note: The between function is from the previous programming assignment 1. 
You can use the between function from the previous assignment for testing purposes. 
*)


let rec safe_zip_int (ls1: int list) (ls2: int list) : ((int * int) list) option = 
  let rec aux  a b accum = 
    match a, b with 
      [],[] -> Some accum
    |
      _::_, [] -> None
    |
      [], _::_ -> None
    |
      h1::t1, h2::t2-> aux t1 t2 (accum@[(h1, h2)])

  in (aux ls1 ls2 [])



(*
Write a function that produces the ith Pell number:
https://en.wikipedia.org/wiki/Pell_number
https://oeis.org/A000129
your function must be tail recursive, and needs to have the correct output up to integer overflow

pell 0 = 0
pell 1 = 1
pell 7 = 169
pell 1000000  does not stack overflow
*)


let rec pell (i: int) : int =
  if i ==0 then 0
  else (if i ==1 then 1
        else let rec aux i prev1 prev2 accum = 
               if i == 1 then accum
               else aux (i-1) (2*prev1+prev2) (prev1) (2*prev1+prev2)
          in aux i 1 0 0)


(* The nth Tetranacci number T(n) is mathematically defined as follows.
 *
 *      T(0) = 0
 *      T(1) = 1
 *      T(2) = 1
 *      T(3) = 2
 *      T(n) = T(n-1) + T(n-2) + T(n-3) + T(n-4)
 *
 * For more information, you may consult online sources.
 *
 *    https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers
 *    https://mathworld.wolfram.com/TetranacciNumber.html
 *
 * Write a tail recursive function tetra that computes the nth Tetranacci
 * number efficiently. In particular, large inputs such as (tetra 1000000)
 * should neither cause stackoverflow nor timeout.
*)

let tetra (n : int) : int = 
  if n < 2 then n
  else (if (n<4) then (n-1)
        else let rec aux i prev1 prev2 prev3 prev4 accum = 
               if i = 4 then prev1+prev2+prev3+prev4
               else aux (i-1) (prev1+prev2+prev3+prev4) prev1 prev2 prev3 (prev1+prev2+prev3+prev4)
          in aux n 2 1 1 0 4)
(*
infinite precision natural numbers can be represented as lists of ints between 0 and 9

Write a function that takes an integer and represents it with a list of integers between 0 and 9 where the head 
of the list holds the least signifigant digit and the very last element of the list represents the most significant digit.
If the input is negative return None. We provide you with some use cases:

For example:
toDec 1234 = Some [4; 3; 2; 1]
toDec 0 = Some []
toDec -1234 = None
*)

(* Hint use 
   mod 10
   / 10
*)

let rec toDec (i : int) : int list option = 
  if i < 0 then None
  else let rec aux i accum = 
         if i = 0 then Some accum
         else aux (i/10) (accum @ [i mod 10])
    in aux i []

(*
Write a function that sums 2 natrual numbers as represented by a list of integers between 0 and 9 where the head is the least signifigant digit.
Your function should be tail recursive

sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
sum [1] [9;9;9] = [0; 0; 0; 1]
sum [] [] = []
sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
*)

let rec sum (a : int list) (b : int list) : int list =  
  let rec aux (a: int list) (b: int list) (carry: int) (accum:int list) : int list = 
    match (a, b) with 
      ([], []) -> (if carry = 0 then accum else accum@[1])
    |
      (h1::t1), [] -> aux t1 b ((h1+carry)/10) accum@[((h1+carry) mod 10)]
    |
      [], h2::t2 -> aux [] t2 ((h2+carry)/10) accum@[((h2+carry) mod 10)]
    |
      h1::t1, h2::t2 -> aux t1 t2 ((h1+h2+carry)/10) accum@[(h1+h2+carry) mod 10]
  in List.rev (aux a b 0 [])


(*
Write an infinite precision version of the pel function from before

pell2 0 = []
pell2 1 = [1]
pell2 7 = [9; 6; 1]
pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]

Hint: You may want to use the sum function from above again inside 
pell2. 

*)

  
let rec pell2 (i: int) : int list = 
  if i ==0 then []
  else (if i ==1 then [1]
        else let rec aux i prev1 prev2 accum = 
               if i == 1 then accum
               else aux (i-1) (sum (sum prev1 prev1) prev2) (prev1) (sum (sum prev1 prev1) prev2)
          in aux i [1] [0] [0])


let _ = 
  print_int_tuple_option (safe_zip_int [0;0;0;0] [0;0;0;1]) ();
  print_int_list (sum [] [0;1]) ();
  print_int_list (sum [4; 3; 2; 1] [1;0;1]) ();
  print_int (pell 7);
  print_int (pell 1);
  print_int (tetra 5);
  print_int_list(pell2 0) ()