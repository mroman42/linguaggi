import scala.io.StdIn.readChar


abstract class Operation
case class Exits() extends Operation
case class Delete() extends Operation
case class Insert(c:Char) extends Operation

abstract class Executor {
  def execute(op : Operation) {}
}


class Editor(textin : String) extends Executor {
  var text = textin + " "
  var cursor = textin.length
  var exiting = false

  def console() {
    printed()
    execute(parseOperation())
    if (!exiting) console()
  }

  def parseOperation() : Operation = {
    readChar match {
      case 'q' => return Exits()
      case 'd' => return Delete()
      case 'i' => return Insert(readChar)
    }
  }

  override def execute(op : Operation) {
    op match {
      case Delete() => delete()
      case Insert(c) => insert(c)
      case Exits() => exits()
    }
  }

  def delete() {
    val newtext = new StringBuilder(text)
    newtext.update(cursor,' ')
    text = newtext.toString;
    cursor = cursor - 1
  }

  def insert(in : Char) {
    val newtext = new StringBuilder(text)
    newtext.update(cursor,in)
    text = newtext.toString
    cursor = cursor + 1
  }

  def exits() {
    exiting = true
  }

  def printed() {
    println(text)
  }
}


trait Debug extends Executor {
  override def execute(op : Operation) {
    super.execute(op)
    println(opInfo(op))
  }

  def opInfo(op : Operation) : String = {
    op match {
      case Insert(s) => "Inserts " + s
      case Delete() => "Deletes char"
      case Exits() => "Exits"
    }
  }
}

println("Editor Version 1")
val ed = new Editor("editable")
ed.console()

println("Editor Version 2")
val ed2 = new Editor("utils") with Debug
ed2.console()
