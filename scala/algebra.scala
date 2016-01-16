trait Monoid[T] {
  def zero: T
  def op: (T,T) => T

  def mconcat(list:List[T]) : T = list.fold(zero) {op}
}

trait Group[T] extends Monoid[T] {
  def id : T = zero
  def opt : (T,T) => T = op
  def inv : T => T

  def prod : List[T] => T = mconcat
}

object Plus extends Group[Int] {
  def zero = 0
  def op = (_+_)
  def inv = (-_)
}

object Mult extends Monoid[Int] {
  def zero = 1
  def op = (_*_)
}
