module Matrix : sig
  type 'a matrix
  val eq : 'a matrix -> 'a matrix -> bool
  (*val copy : 'a matrix -> 'a matrix*)
  val combine : 'a matrix -> 'b matrix -> ('a*'b) matrix
  val add : int matrix -> int matrix -> int matrix
  val map : ('a -> 'b) -> 'a matrix -> 'b matrix
  val dimensions : 'a matrix -> (int * int)
  val make : (int -> int -> 'a) -> int -> int -> 'a matrix
  val scalar : int -> int matrix -> int matrix
  (*val prod : matrix -> matrix -> matrix
  val transposition : matrix -> matrix
  val norm : matrix -> int*)
end with type 'a matrix = 'a array array =
struct
  type 'a matrix = 'a array array

  let eq = (=);;

  (*let copy = Array.map (Array.copy);;*)

  let make f h w = 
    let m = (Array.make_matrix h w (f 0 0)) in
    for i=0 to h-1 do
      for j=0 to w-1 do
        m.(i).(j) <- f i j
      done
    done;
    m
  ;;

  let map f = Array.map (Array.map f);;

  let dimensions m = (Array.length m, Array.length m.(1));;
  
  let combine a b =
    if not (dimensions a = dimensions b) then
      raise (Failure "Invalid dimensions")
    else 
      let (h,w) = dimensions a in
      make (fun i j -> (a.(i).(j), b.(i).(j))) h w
  ;;


  (* Aritmetic functions *)
  let add a b = map (fun (x,y) -> x+y) (combine a b);;
  let scalar a m = map (fun x -> a*x) m;;
  let prodm a b = map (fun (x,y) -> x*y) (combine a b);;
  let prod a b = 
    let sum = List.fold_left (+) 0 in 
    make (fun (i,j) -> sum [])
end
