(* Testing Ocaml as a functional programming language*)
let average a b =
  let sum = a +. b in
  sum /. 2.0;;

let my_ref = ref 0;; (* References (!) *)

let negation x = 
  match x with
    | true -> false
    | false -> true
  ;;

let rec factorial n =
  match n with
    | 0 -> 1
    | n -> n*(factorial (n-1))
  ;;

let rec map f list =
  match list with
    | [] -> []
    | x :: xs -> (f x) :: (map f xs) 
;;

(* There are arrays and records*)
let arrayexp = [|1;2;3;5|];;
type person = {name: string; mutable age:int};;
type tree = Nothing | Node of tree;;  

(* () is the unit z l*)
let main () = print_string ("Hello World!\n");;
main ();;

