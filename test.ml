(* Testing Ocaml as a functional programming language*)
let average a b =
  let sum = a +. b in
  sum /. 2.0;;

let my_ref = ref 0;; (* References (!) *)

(* () is the unit *)
let main () = print_string ("Hello World!\n");;
main ();;

