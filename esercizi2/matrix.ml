module Matrix : sig
  type matrix = int array array
  val eq : matrix -> matrix -> bool
  val copy : matrix -> matrix
  val add : matrix -> matrix -> matrix
  val scalar : float -> matrix -> matrix
  val prod : matrix -> matrix -> matrix
  val transposition : matrix -> matrix
  val norm : matrix -> int
end =
struct
  
end
