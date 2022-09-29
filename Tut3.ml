(* higher order list functions *)
(* map, maps the func (or labda expression/anon func) to each element in the list *)
let exclaim (lst : string list) : string list = List.map (fun x -> x ^ "!") lst
let concat_exclaim (s : string) : string = s ^ "!"
let exclaim' (lst : string list) : string list = List.map concat_exclaim lst

let capitalize (lst : string list) : string list =
  List.map (fun s -> String.capitalize_ascii s) lst

(* filter *)
(* return only odd integers *)
let odd (lst : int list) : int list = List.filter (fun x -> x mod 2 != 0) lst

(* return only capitalized *)
let is_capital (s : string) : bool =
  String.get s 0 = Char.uppercase_ascii (String.get s 0)

let only_capitalized (lst : string list) : string list =
  List.filter is_capital lst

(* fold left and fold right *)
(*
fold_left ( + ) 0 [1;2;3]
fold_left ( + ) (0 + 1) [2;3]
...
*)
let suml (lst : int list) : int = List.fold_left ( + ) 0 lst

(*
fold_right ( + ) [1;2;3] 0
( + ) 1 (fold_right ( + ) [2;3] 0)
1 + (fold_right ( + ) [2;3] 0)
...
*)
let sumr (lst : int list) : int = List.fold_right ( + ) lst 0
let add1 (curr_sum : int) (_ : 'a) : int = curr_sum + 1
let length (lst : 'a list) : int = List.fold_left add1 0 lst

(* return the sum of the even positions in the lst *)
let even_pos_sum (lst : int list) : int =
  (* this time the accumulator is a tuple (0,0), both start at 0 *)
  let f (sum, pos) element : 'a * 'b =
    if pos mod 2 = 0 then (sum + element, pos + 1) else (sum, pos + 1)
  in
  fst (List.fold_left f (0, 0) lst)

(* mult all floats in a float list *)
let productf (lst : float list) : float =
  (* accumulator needs to be 1.0 instead of 0.0 because
     if we start with 0.0 .* [lst] then the product will be 0.0
  *)
  List.fold_left ( *. ) 1.0 lst

(* smallest element in a list *)
let min (lst : 'a list) : 'a =
  let m min_so_far elem = if elem > min_so_far then min_so_far else elem in
  match lst with
  | [] -> invalid_arg "minimum, empty list"
  | e :: rest -> List.fold_left m e lst
