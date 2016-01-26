import scala.io.StdIn.readChar
import scala.collection.mutable.Stack

abstract class Operation
case class Exits() extends Operation
case class Delete() extends Operation
case class Insert(c:Char) extends Operation

abstract class Executor {
  var text : String
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


trait UndoRedo extends Executor {
  var undos = Stack[String]()
  var redos = Stack[String]()

  override def execute(op : Operation) {
    undos.push(text)

    super.execute(op)

    println("Undo or Redo?")
    readChar match {
      case 'u' => Undo()
      case 'r' => Redo()
      case _ => {}
    }
  }

  def Undo() {
    redos.push(text)
    text = undos.pop
  }

  def Redo() {
    undos.push(text)
    text = redos.pop
  }
}


println("Editor Version 1")
val ed = new Editor("editable")
ed.console()

println("Editor Version 2")
val ed2 = new Editor("utils") with UndoRedo with Debug
ed2.console()
