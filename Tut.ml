let increment x = x + 1
let increment' (x : float) : float = x +. 1.
let add (x : int) (y : int) : int = x + y

(* The else branch can be omitted, but in this case it is implicitly replaced by else (). Consequently, the type of the expression expr2 must be unit *)
let rec fib (x : int) : int =
  if x = 0 then 0 else if x < 3 then 1 else fib (x - 2) + fib (x - 1)

(* Write a function named circle_area
   it will take in argument 'r' (radius) and
   returns the area of the circle *)
let pi = 3.14
let circle_area (r : float) : float = r *. r *. pi

(* Write a function name power that takes in
   a power 'n' and float 'x' and returns x ^ n *)
let power (n : int) (x : float) : float = x ** float_of_int n

let rec power' (n : int) (x : float) : float =
  if n = 0 then 1.0 else x *. power (n - 1) x

let square (x : float) = power 2 x

(* Write a function named gcd that computes
   the greatest common divisor of two positive integers *)
let gcd (x : int) (y : int) : int =
  let min = if x < y then x else y in
  let rec dec m = if x mod m = 0 && y mod m = 0 then m else dec (m - 1) in
  dec min
