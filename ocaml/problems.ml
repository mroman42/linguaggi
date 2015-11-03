let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t
;;

assert (last [`a; `b; `c; `d] = Some `d);;
assert (last [] = None)


let rec last_two = function
    | [] -> None
    | [x] -> None
    | [x;y] -> Some x
    | _ :: t -> last_two t
;;

assert (last_two [5;1;2;3] = Some 2);;


let rec at k = function
    | [] -> None
    | x :: t -> if k = 0 then Some x else (at (k-1) t)
;;

assert (at 3 [5;2;4;7;4] = Some 7)


(* Primes *)
let rec range = function
    | 1 -> []
    | n -> n :: (range (n-1))
;;

let divisor a b = (a mod b = 0);;
let any = List.fold_left (||) false;;
let is_prime n = any (List.map (divisor n) (range (n-1)));;
