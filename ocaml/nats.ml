(* Natural numbers *)
module type NaturalStructure =
  sig 
    type nat
    val zero : nat
    val succ : nat -> nat
    val pred : nat -> nat
  end;;

module Naturals : NaturalStructure =
  struct
    type nat = Z | S of nat;;
    let succ n = S n;;
    let zero = Z;;
     
    let pred =
      function Z -> Z
             | S n -> n
    ;;



    let rec toint = 
      function Z   -> 0
             | S n -> 1 + (toint n)
    ;;
end;;

module IntNats : NaturalStructure 
       with type nat = int 
= 
  struct
    type nat = int;;
    let succ = fun x -> x+1;;
    let zero = 0;;
    let pred = 
      function 0 -> 0
             | n -> n-1
    ;;
end;;


module NaturalArithmetic (Nats : NaturalStructure) =
  struct 
    let rec add a b = if (b == Nats.zero)
                      then a 
                      else Nats.succ (add a (Nats.pred b));;
  end;;

module IntArithm = NaturalArithmetic (IntNats);;
