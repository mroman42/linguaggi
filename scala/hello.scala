// An object is a class with a single instance
object Upper {
  def upper(strings:String*) = strings.map(_.toUpperCase())
}

println(Upper.upper("A","First","Scala","Program").mkString(" "))
