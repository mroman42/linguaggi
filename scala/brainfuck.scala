import scala.util.parsing.combinator._

class BrainfuckParser extends JavaTokenParsers {
  // Turing Machine
  val size = 30000
  var pointer = 0 
  var machine = Array.fill[Byte](size)(0)

  // Brainfuck grammar
  // Defines the grammar and parses the tokens.
  // (!) The type signature is needed in 'statement' because it is a 
  // (!) recursive function.
  def program = statementlist
  def statementlist = (statement *)
  def statement : Parser[java.io.Serializable] = {
    next | prev | inc | dec | out | in | loop | spurious
  }

  def spurious = """[a-zA-Z]*""".r ^^^ "comment"
  def inc = "+" ^^ { s => INC() }
  def dec = "-" ^^ { s => DEC() }
  def next = ">" ^^ { s => NEXT() }
  def prev = "<" ^^ { s => PREV() }
  def out = "." ^^ { s => OUT() }
  def in = "," ^^ { s => IN() }
  def loop = "[" ~> (statement *) <~ "]" 

  // Token definition
  trait Token
  case class INC() extends Token
  case class DEC() extends Token
  case class NEXT() extends Token
  case class PREV() extends Token
  case class OUT() extends Token
  case class IN() extends Token
  case class LOOP(val tokens: Seq[Token]) extends Token

  // Execution
  // Runs a list of tokens
  def run (input : Seq[Token]) {
    for (token <- input) {
      token match {
        case INC() => { machine(pointer) = (machine(pointer) + 1).toByte }
        case DEC() => { machine(pointer) = (machine(pointer) - 1).toByte }
        case NEXT() => { pointer = pointer + 1 }
        case PREV() => { pointer = pointer - 1}
        case OUT() => { println(machine(pointer).toChar) }
        case IN() => { machine(pointer) = readByte() }
        case LOOP(tokens) => { do { run(tokens) } while (machine(pointer) != 0)}
      }
    }
  }
}


val b = new BrainfuckParser

