(* lists *)
let l = [ 2; 3 ] (* construct list of 2 and 3 *)

(* list of tuples *)
let l2 = [ ("Name1", 13); ("Name2", 23) ]
let int_list = [ 2; 3; 4 ] (* another way to build a list *)

let int_list =
  1 :: int_list (* prepend 1 to the list by shadowing the variable *)

(* concat lists *)
let l3 = [ 1; 2; 3 ] @ l

(* pattern matching *)
let rec all_true (lst : bool list) : bool =
  match lst with
  | [] -> true (* base case *)
  | x :: [] ->
      if x = true then true else false (* just one element in the list *)
  | x :: rest -> x && all_true rest
(* if x which is the head element is true and the rest of the list used for recursion *)

(* check if both the elements num and the elements themselves are even *)
let even x = x mod 2 = 0

let rec even2ways (lst : int list) : bool =
  match lst with
  | [] -> true (* base case *)
  | x :: [] -> false (* list with one element is not even *)
  | x1 :: x2 :: rest -> even x1 && even x2 && even2ways rest

(* returns true if the list is empty, otherwise false
   'a means that the list can be any list, i.e int list, etc...
*)
let is_empty (lst : 'a list) : bool =
  match lst with
  | [] -> true
  | _ :: _ -> false (* 2 whatever make a list, so it's false *)

(* function to return the head of a list *)
let l = [ 1; 2; 3 ]

let head (lst : 'a list) : 'a =
  match lst with
  | x :: _ -> x
  (* raise exception *)
  | _ -> raise (invalid_arg "head")

(* function that adds all the numbers in a list *)
let rec sum_int_list (lst : int list) : int =
  match lst with [] -> 0 | x :: [] -> x | x :: rest -> x + sum_int_list rest

(* function that finds the smallest element in a list *)
let rec min (lst : 'a list) : 'a =
  match lst with
  | [] -> raise (invalid_arg "min, empty list")
  | x :: [] -> x
  | x :: rest -> if x < min rest then x else min rest

(* function that appends two lists *)
let rec append_lists (xs : 'a list) (ys : 'a list) : 'a list =
  match xs with
  | [] -> ys
  | x :: [] -> x :: ys
  | x :: rest -> x :: append_lists rest ys

(* function that determines if an element is contained in a list *)
let rec is_in_list (e : 'a) (xs : 'a list) : bool =
  match xs with [] -> false | x :: rest -> x = e || is_in_list e rest

(* function that concatenates '!' to every string in a list *)
let rec concat_esclamation (xs : string list) : string list =
  match xs with
  | [] -> []
  | x :: [] -> (x ^ "!") :: []
  | x :: rest -> (x ^ "!") :: concat_esclamation rest

(* function that takes a list of tuples and unzips them into a tuple of lists *)
(* i.e [("hello",1);("hey",2);("ciao",3)] becomes
   (["hello";"hey";"ciao"];[1;2;3])
*)

let rec unzip_list_2_tuple (lst : ('a * 'b) list) : 'a list * 'b list =
  match lst with
  | [] -> ([], [])
  | (x, y) :: [] -> (x :: [], y :: [])
  | [ (x, y); (x2, y2) ] -> ([ x; x2 ], [ y; y2 ])
  | (x, y) :: rest ->
      (x :: fst (unzip_list_2_tuple rest), y :: snd (unzip_list_2_tuple rest))
