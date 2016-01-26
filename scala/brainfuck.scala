import scala.util.parsing.combinator._
import scala.language.postfixOps

class BrainfuckParser extends JavaTokenParsers {
  // Turing Machine
  val size = 30000
  var pointer = 0
  var machine = Array.fill[Byte](size)(0)

  // Brainfuck grammar
  // Defines the grammar and parses the tokens.
  // (!) The type signature is needed in 'statement' because it is a 
  // (!) recursive function.
  def program = (spurious *) ~> statementlist
  def statementlist = (statement *)
  def statement : Parser[Token] = operation <~ (spurious *) 
  def operation : Parser[Token] = (next | prev | inc | dec | out | in | loop)

  def spurious = """[^.,+<>\-\[\]]""".r
  def inc : Parser[Token] = "+" ^^ { s => INC() }
  def dec : Parser[Token] = "-" ^^ { s => DEC() }
  def next : Parser[Token] = ">" ^^ { s => NEXT() }
  def prev = "<" ^^ { s => PREV() }
  def out = "." ^^ { s => OUT() }
  def in = "," ^^ { s => IN() }
  def loop = "[" ~> (spurious *) ~> (statement *) <~ "]" ^^ { s => LOOP(s) }

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
        case OUT() => { print(machine(pointer).toChar) }
        case IN() => { machine(pointer) = scala.io.StdIn.readByte() }
        case LOOP(tokens) => { do { run(tokens) } while (machine(pointer) != 0) }
      }
    }
  }

  // Parsing and execution unified
  def parseandrun(program : String) {
    b.parseAll(b.program, program) match {
      case b.Success(parsed, _) =>
        b.run(parsed)
      case b.NoSuccess(msg, _) =>
        throw new IllegalArgumentException("Parsing Error: " + msg)
    }
  }
}


val b = new BrainfuckParser
val helloworld = """
+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set the next four cells to 70/100/30/10
> +++++ ++              add  7 to cell #1
> +++++ +++++           add 10 to cell #2
> +++                   add  3 to cell #3
> +                     add  1 to cell #4
<<<< -                  decrement counter (cell #0)
]
> ++ .                  print ’H’
> + .                   print ’e’
+++++ ++ .              print ’l’
.                       print ’l’
+++ .                   print ’o’
> ++ .                  print ’ ’
<< +++++ +++++ +++++ .  print ’W’
> .                     print ’o’
+++ .                   print ’r’
----- - .               print ’l’
----- --- .             print ’d’
> + .                   print ’!’
> .                     print ’\n’

"""

b.parseandrun(helloworld)
