import scala.annotation.tailrec

object Basic {
  def is_palindrome(string:String) : Boolean = {
    string == string.reverse
  }

  // Prime factors of a number
  def factors(n:Int) : List[Int] = {
    @tailrec
    def factoraux(n:Int, a:Int=2, acc:List[Int]=Nil): List[Int] =
      a*a>n match {
        case false if (n%a == 0) => factoraux(n/a, a, a::acc)
        case false               => factoraux(n, a+1, acc)
        case true                => n::acc
      }
    factoraux(n)
  }

  // Divisors of a number, as a list
  def divisors(n:Int) : List[Int] = {
    List.range(1,n-1).filter(n%_==0)
  }

  def is_perfect(n:Int) : Boolean = {
    divisors(n).sum == n
  }
}
