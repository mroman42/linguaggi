(* Natural numbers *)
type nat = Z | S of nat;;

let succ n = S n;;

let rec add a = 
  function Z -> a 
         | S b -> S (add a b)
;;

let rec prod a = 
  function Z   -> Z
         | S b -> add (prod a b) a
;;

let rec toint = 
  function Z   -> 0
         | S n -> 1 + (toint n)
;;

