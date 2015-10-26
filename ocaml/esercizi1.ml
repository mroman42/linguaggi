(** EXERCISES ON ML/OCaML **)

(* Exercise 1 *)
let alkaline_earth_metals = [38;12;4;20;56;88];;

let rec max l = match l with
    | [] -> 0
    | h::t -> if h > (max t) then h else (max t)
;;

let rec quicksort1 l = match l with
    | [] -> []
    | h::t -> let smaller = quicksort1 (List.filter (fun x -> x < h) t)  in 
              let greater = quicksort1 (List.filter (fun x -> x >= h) t) in  
              smaller @ [h] @ greater;;

let rec quicksort comp l = match l with
    | [] -> []
    | h::t -> (quicksort comp (List.filter (fun x -> comp x h) t)) @ [h] @ (quicksort comp (List.filter (fun x -> not (comp x h)) t))
;;

let noble_gases = [2;10;18;36;54;86];;
let noble_gases_names = ["helium";"neon";"argon";"krypton";"xenon";"radon"];;

let merge_atomic names numbers = quicksort1 (List.combine names numbers)



(* Exercise 2 *)
(* This defines an affine transformation, of the form: ax+b. 
   It can be evaluated for a particular value of x, and its
   inverse can be computed. *)
type affine = {a:float; b:float};;
let eval aff t = (aff.a *. t) +. aff.b;;
let inverse aff = {a=1.0 /. aff.a; b= 0.0 -. (aff.b) *. (1.0 /. aff.a)};;

type scale = Kelvin | Celsius | Fahrenheit | Rankine | Delisle | Newton | Reaumur | Romer;;

let toKelvin scale = match scale with
    | Kelvin ->     {a=1.0; b=0.0}
    | Celsius ->    {a=1.0; b=(-273.15)}
    | Fahrenheit -> {a=(5.0 /. 9.0); b=((5.0 /. 9.0) *. 459.67)} 
    | Rankine ->    {a=1.0; b=(5.0 /. 9.0)}
    | Delisle ->    {a=(0.0 -. (2.0 /. 3.0)); b=373.15}
    | Newton ->     {a=(100.0 /. 33.0); b=273.15} 
    | Reaumur ->    {a=(5.0 /. 4.0); b=273.15}
    | Romer ->      {a=(40.0 /. 21.0); b=((-2.5) *. (40.0 /. 21.0) +. 273.15)}
;;

(* Transforms between to scales thru the Kelvin scale. *)
let transform fromscale toscale t = eval (toKelvin toscale) (eval (inverse (toKelvin fromscale)) t);;


(**)
