class Matrix(val n:Int, val m:Int, val fun: (Int,Int) => Int) {
  // Matrix creation from an index function
  private val matrix : List[List[Int]] = List.tabulate[Int](n,m)(fun)

  // Componentwise operations
  def componentwise(op:(Int,Int)=>Int)(other:Matrix) : Matrix = {
    // Exception to ensure correct dimensions
    if (n!=other.n || m != other.m) {
      throw new IllegalArgumentException("Sizes of matrices do not match")
    }
    new Matrix(n,m, (i,j) => op(matrix(i)(j),other.matrix(i)(j)))
  }

  def + = componentwise({_+_}) _
  def - = componentwise(_-_) _

  // Pretty printing
  override def toString = {
    matrix
      .map(_.fold(""){_ + _.toString() + " "})
      .foldLeft(""){_ + _ + "\n"}
  }
}


// Unit Tests
val a = new Matrix(2,4,{_+_})
val b = new Matrix(2,4,{_*_})
a + b
