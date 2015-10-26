module Matrix : sig
  type 'a matrix
  val eq : 'a matrix -> 'a matrix -> bool
  (*val copy : 'a matrix -> 'a matrix*)
  val combine : 'a matrix -> 'b matrix -> ('a*'b) matrix
  val add : int matrix -> int matrix -> int matrix
  val map : ('a -> 'b) -> 'a matrix -> 'b matrix
  val dimensions : 'a matrix -> (int * int)
  (*val scalar : float -> matrix -> matrix
  val prod : matrix -> matrix -> matrix
  val transposition : matrix -> matrix
  val norm : matrix -> int*)
end with type 'a matrix = 'a array array =
struct
  type 'a matrix = 'a array array
  let eq = (=);;

  (*let copy = Array.map (Array.copy);;*)

  let map f = Array.map (Array.map f);;
  let dimensions m = (Array.length m, Array.length m.(1));;
  
  let combine a b =
    if (Matrix.dimensions a != Matrix.dimensions b) then
      raise (Failure "Invalid dimensions")
    else
      let combinearray a b = if Array.length a == Array.length b then
                               Array.mapi (fun i x -> (x,b.(i))) (Array.copy a)
                             else (raise (Failure "Invalid dimensions"))
      in
      Array.mapi (fun i x -> combinearray x (b.(i))) (Array.copy a)
  ;;

  let add a b = 
    Matrix.map (fun (x,y) -> x+y) (combine a b)
  ;;
end
