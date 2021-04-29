(*
Honor code comes here:
First Name:
Last Name:
BU ID:
I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(* the type of a plymorphic tree *)
type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree


(*
TODO: write a map function for trees:
For example,
map_tree (fun x -> x+1) (Node (Leaf 1, Leaf 2)) =  (Node (Leaf 2, Leaf 3))
map_tree (fun _ -> 0)  (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
                       (Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Leaf 0    )))
*)
let rec map_tree (f: 'a -> 'b) (tree: 'a tree): 'b tree = 
  match tree with
    Leaf a -> Leaf (f a)
  |
    Node (left, right) -> Node((map_tree f left), (map_tree f right))

(*
TODO: write a fold function for trees:
*)

let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  = 
  match tree with
    Leaf a -> leaf a
  |
    Node (left, right) -> node (fold_tree node leaf left) (fold_tree node leaf right)



(*
TODO: sum the contents of an int tree
For example,
sum_ints (Node (Leaf 1, Leaf 2)) = 3
*)
let rec sum_ints (tree: int tree): int  = 
  fold_tree (fun x y -> x+y) (fun z->z) tree



(*
TODO: find the size of the tree
For example,
tree_size (Leaf 1) = 1
tree_size (Node (Leaf 1, Leaf 2)) = 3
*)
let rec tree_size (tree: 'a tree): int  =
  fold_tree (fun x y -> 1+x+y) (fun x->1) tree

(*
TODO: find the height of the tree
For example,
tree_height (Leaf 2) = 1
tree_height (Node ((Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))), Leaf 2)) = 5
*)
let rec tree_height (tree: 'a tree): int  = 
  fold_tree (fun x y -> if x>y then x+1 else y+1) (fun z -> 1) tree

(*
TODO: write a function that takes a predicate on trees and retuns true if any subtree satisfies that predicate
For example,
tree_contains (Node (Leaf 1, Leaf 2)) (fun x -> match x with Leaf 2 -> true | _ -> false) = true
*)
let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool  = 
  match tree with 
    Leaf l -> look_for (Leaf l)
  |
    Node (left, right) -> look_for (Node(left, right))||(tree_contains left look_for)||(tree_contains right look_for)


(*
TODO: write a function that shows bool trees :
For example,
show_bool_tree (Leaf true) ="true"
show_bool_tree (Node (Leaf true, Leaf false)) = "(true^false)" 
show_bool_tree  (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
    "((true^(true^false))^((true^(true^false))^false))" 
*)
let rec show_bool_tree (tree: bool tree) : string  = 
  fold_tree(fun x y -> "("^x^"^"^y^")") (fun z -> if z then "true" else "false") tree 

(* standard functions to convert between strin and char list *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

(*
TODO: write a fubction that reads bool trees :
for all (finite) t : bool trees.
read_bool_tree t = Some (show_bool_tree t)
For example,
read_bool_tree "true" = Some (Leaf true)
read_bool_tree "false" = Some (Leaf false)
read_bool_tree "tralse" = None
read_bool_tree "(true^false)" = Some (Node (Leaf true, Leaf false))
read_bool_tree "((true^(true^false))^((true^(true^false))^false))" =
Some
 (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false)))
*)

(* Hint 
   write a helper function 
   read_bool_prefix : (char list) -> ((bool * (char list)) option) 
   such that
   read_bool_prefix (explode "true???")       = Some (true, ['?'; '?'; '?'])
   read_bool_prefix (explode "false123")      = Some (false, ['1'; '2'; '3'])
   read_bool_prefix (explode "antythingales") = None
   read_bool_prefix []                        = None
   write a helper function 
   read_bool_tree_prefix (char list) -> ((bool tree * (char list)) option) 
   such that
   read_bool_tree_prefix [] = None
   read_bool_tree_prefix (explode "true???") = Some (Leaf true, ['?'; '?'; '?'])
   read_bool_tree_prefix (explode "(true^false)124") = Some (Node (Leaf true, Leaf false), ['1'; '2'; '4'])
   read_bool_tree_prefix (explode "(true^(true^false))aaa") = Some (Node (Leaf true, Node (Leaf true, Leaf false)), ['a'; 'a'; 'a'])
   read_bool_tree_prefix (explode "(true^(true^fa se))aaa") = None
*)


let read_bool_prefix (ls: char list): ((bool*char list) option) = 
  match ls with
  't' :: 'r' :: 'u' :: 'e' :: rest -> Some (true, rest)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> Some (false, rest)
  | _ -> None

let rec read_bool_tree_prefix(ls: char list) (left: char list): ((bool tree option )) =
  match ls with
  []->None
  |
  '^'::t -> Some Node( (read_bool_tree_prefix left []), (read_bool_tree_prefix t []))
  |
  '('::t -> read_bool_tree_prefix t left
  |
  
  h::t -> read_bool_tree_prefix t ([h]@left)


    


let rec read_bool_tree (tree: string) : ((bool tree) option) = failwith "unimplemented"
(*
write a fubction that checks that parenthisis are balnaced:
Parenthisis are balenced if there are no parenthises
Parenthisis are balenced if ( and )  enclose a balenced parenthises
Parenthisis are balenced if balenced parenthises are ajacent to a balenced parenthisis
For example,
matching_parens "" = true
matching_parens "((((((((((()))))))))))" = true
matching_parens "()()()()()()" = true
matching_parens "(()())" = true
matching_parens "())(()" = false
*)


(* Hint 
   write mutually recirsive functions 
   matching_paren_prefix : (char list) -> ((char list) option)
   matching_parens_prefix : (char list) -> ((char list) option)
   the and keyword allows mutual recursion
   let rec matching_paren_prefix (ls: char list) : ((char list) option) = failwith "unimplemented"
   and matching_parens_prefix  (ls: char list) : ((char list) option) = failwith "unimplemented"
   such that
   matching_paren_prefix [] = None
   matching_paren_prefix (explode "(???") = None
   matching_paren_prefix (explode "()???") = Some ['?'; '?'; '?']
   matching_paren_prefix (explode "(((())))123") = Some ['1'; '2'; '3']
   matching_paren_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
   matching_paren_prefix (explode "(()()())abc") = Some ['a'; 'b'; 'c']
   matching_parens_prefix [] = Some []
   matching_parens_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
   matching_parens_prefix (explode ")aa") = Some [')'; 'a'; 'a']
*)


let rec matching_parens (tree: string) : bool  =
  let rec aux s count = 
    if count >= 0 then
      (match s with 
         [] -> count
       |
         '('::t -> aux t (count+1)
           | 
             ')'::t -> aux t (count-1)
           |
             _::t -> aux t count)
    else 15
  in  if (aux (explode tree) (0)) = 0 then true else false
