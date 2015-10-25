module Matrix : sig
  type 'a matrix
  val eq : 'a matrix -> 'a matrix -> bool
  (*val copy : 'a matrix -> 'a matrix*)
  val combine : 'a matrix -> 'b matrix -> ('a*'b) matrix
  val add : int matrix -> int matrix -> int matrix
  (*val scalar : float -> matrix -> matrix
  val prod : matrix -> matrix -> matrix
  val transposition : matrix -> matrix
  val norm : matrix -> int*)
end with type 'a matrix = 'a array array =
struct
  type 'a matrix = 'a array array
  let eq = (=);;
  (*let copy = Array.map (Array.copy);;*)

    
  let combine a b =
    if (Array.length a == Array.length b) then
      let combinearray a b = if Array.length a == Array.length b then
                               Array.mapi (fun i x -> (x,b.(i))) (Array.copy a)
                             else (raise (Failure "Invalid dimensions"))
      in
      Array.mapi (fun i x -> combinearray x (b.(i))) (Array.copy a)
    else (raise (Failure "Invalid dimensions"))
  ;;

  let add a b = 
    Array.map (Array.map (fun (x,y) -> x+y)) (combine a b)
  ;;


end
