(* inductive types *)
(* parametric polymorphism, i.e the 'a or 'b type *)
type msg =
  | StringMsg of string * int
  | BoolMsg of bool * int
  | FloatMsg of float * int

let sample_msgs : msg list =
  [
    StringMsg ("Hello", 0000);
    StringMsg ("World", 1200);
    BoolMsg (true, 1300);
    FloatMsg (2.42, 1330);
  ]

let msg_log (m : msg) : string =
  match m with
  | StringMsg (s, ts) -> "Msg at " ^ string_of_int ts ^ ": " ^ s
  | BoolMsg (b, ts) -> "Msg at " ^ string_of_int ts ^ ": " ^ string_of_bool b
  | FloatMsg (f, ts) -> "Msg at " ^ string_of_int ts ^ ": " ^ string_of_float f

let log_messages (msgs : msg list) : string =
  String.concat "\n" (List.map msg_log msgs)

let print_messages (msgs : msg list) = print_endline (log_messages msgs)

(* binary tree *)
type 'a btree = Empty | Node of 'a btree * 'a * 'a btree

(*
    3
  2   4
        5
*)
let bt1 : int btree =
  Node (Node (Empty, 2, Empty), 3, Node (Empty, 4, Node (Empty, 5, Empty)))

let bt2 : string btree =
  Node
    (Node (Empty, "A", Empty), "B", Node (Empty, "E", Node (Empty, "D", Empty)))

let bt3 : 'a btree = Empty

(* function to sum all the nodes of an int btree *)
let rec sumbtree (bt : int btree) : int =
  match bt with
  | Empty -> 0
  | Node (left, x, right) -> sumbtree left + x + sumbtree right

(* convert btree to a list *)
let rec btree2list (bt : 'a btree) : 'a list =
  match bt with
  | Empty -> []
  | Node (left, x, right) -> btree2list left @ (x :: []) @ btree2list right

(* tree map function, apply an operation to every node in the tree *)
let rec map (f : 'a -> 'b) (bt : 'a btree) : 'b btree =
  match bt with
  | Empty -> Empty
  | Node (left, x, right) -> Node (map f left, f x, map f right)

let square_tree (t : int btree) : int btree = map (fun x -> x * x) t

(* takes an accumulator of type 'b, a curried function to apply the fold
   and a btree of type 'a and returns a type 'b

   utop # List.map;;
   - : ('a -> 'b) -> 'a list -> 'b list = <fun>
   ─( 09:17:25 )─< command 16 >──────────────────────────────────────────────────────────────────{ counter: 0 }─
   utop # map;;
   - : ('a -> 'b) -> 'a btree -> 'b btree = <fun>
*)
let rec foldl_tree (e : 'b) (n : 'b -> 'a -> 'b -> 'b) (t : 'a btree) : 'b =
  match t with
  | Empty -> e
  | Node (left, x, right) -> n (foldl_tree e n left) x (foldl_tree e n right)

let product (t : int btree) : int =
  (* the accumulator needs to be 1, not 0, because we are taking the product *)
  foldl_tree 1 (fun left x right -> left * x * right) t

type 'a rosetree = Rose of 'a * 'a rosetree list

let rt1 : int rosetree =
  Rose
    (2, [ Rose (1, [ Rose (5, []); Rose (7, []) ]); Rose (3, []); Rose (4, []) ])

let rec rt_fold (r : 'a -> 'b list -> 'b) (t : 'a rosetree) : 'b =
  match t with Rose (x, list) -> r x (List.map (rt_fold r) list)

let sum (t : int rosetree) : int =
  let f (x : 'a) (xs : int list) : int =
    x + (fun ys -> List.fold_right ( + ) ys 0) xs
  in
  rt_fold f t
