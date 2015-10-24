(* Erasthothenes' sieve. Computes the prime numbers. *)
let rec range a b =
  if a > b 
  then []
  else a :: range (a+1) b
;;

let rec sieve = 
  let remove_mult n t = List.filter (fun m -> m mod n <> 0) t in 
  function
    | []     -> []
    | n :: t -> n :: sieve (remove_mult n t)
;;

let primes n = sieve (range 2 n);;


(* Goldbach partitions *)
let goldbach n = 
  let nprimes = primes n in
  let isprime x = List.exists (fun y -> y == x) nprimes in
  List.find (fun x -> isprime (snd x)) (List.map (fun x -> (x,n-x)) nprimes)
;;

let goldbach_list n m = 
  let evenrange = List.filter (fun x -> x mod 2 == 0) (range n m) in
  List.map goldbach evenrange
;; 
