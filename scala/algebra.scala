trait Monoid[T] {
  def zero: T
  def op: (T,T) => T

  def mconcat(list:List[T]) : T = list.fold(zero) {(a,b)=>op(a,b)}
}

object PlusMonoid extends Monoid[Int] {
  def zero = 0
  def op = (_+_)
}
