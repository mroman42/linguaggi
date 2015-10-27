module Matrix : sig
  type 'a matrix
  val eq : 'a matrix -> 'a matrix -> bool
  val copy : 'a matrix -> 'a matrix
  val combine : 'a matrix -> 'b matrix -> ('a*'b) matrix
  val add : int matrix -> int matrix -> int matrix
  val map : ('a -> 'b) -> 'a matrix -> 'b matrix
  val dimensions : 'a matrix -> (int * int)
  val scalar : int -> int matrix -> int matrix
  val prod : int matrix -> int matrix -> int matrix
  val transposition : 'a matrix -> 'a matrix
end with type 'a matrix = 'a array array =
struct
  type 'a matrix = 'a array array
                      
  (* Equality can be used directly *)
  let eq = (=);;

  (* Private function. Given a function over the
     indices, returns a new matrix. *)
  let make f h w = 
    let m = (Array.make_matrix h w (f 0 0)) in
    for i=0 to h-1 do
      for j=0 to w-1 do
        m.(i).(j) <- f i j
      done
    done;
    m
  ;;

  (* Higher map, better suited for matrices. *)
  let map f = Array.map (Array.map f);;

  (* Return the dimensions of the matrix.
     It assumes all rows to be the same length! *)
  let dimensions m = (Array.length m, Array.length m.(1));;

  (* Matrix functions *)
  (* Returns a new identical matrix. *)
  let copy m = make (fun i j -> m.(i).(j)) (fst (dimensions m)) (snd (dimensions m));;
  (* Returns the transposed matrix. *)
  let transposition m = make (fun i j -> m.(j).(i)) (snd (dimensions m)) (fst (dimensions m));;

  (* Combines two matrices. Analogue for the function zip in Haskell.
     It is an auxiliar function for the arithmetic ones. *)
  let combine a b =
    if not (dimensions a = dimensions b) then
      raise (Failure "Invalid dimensions")
    else 
      let (h,w) = dimensions a in
      make (fun i j -> (a.(i).(j), b.(i).(j))) h w
  ;;


  (* Arithmetic functions *)
  let add a b = map (fun (x,y) -> x+y) (combine a b);;
  let scalar a m = map (fun x -> a*x) m;;
  let prodm a b = map (fun (x,y) -> x*y) (combine a b);;
  let prod a b = 
    let sum = List.fold_left (+) 0 in
    let rec range a b = if a == b then [] else a :: range (a+1) b in
    let dima = (dimensions a) in
    let dimb = (dimensions b) in
    let lst i j = List.map (fun x -> a.(i).(x) * b.(x).(j)) (range 0 (snd dima)) in

    if (snd dima == fst dimb) 
    then make (fun i j -> sum (lst i j)) (fst dima) (snd dimb)
    else raise (Failure "Invalid dimensions")
  ;;


end
