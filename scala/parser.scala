class SimpleParser extends JavaTokenParsers {
  var counter = 0
  def cosas = (alfanumericos *)
  def alfanumericos = """[a-zA-Z]""".r ^^ { s => {counter = counter+1} }
  def printCounter = println(counter)
}

val parser = new SimpleParser
val cadena = "asdfaasdfasdfsdf

parser.parseAll(parser.cosas,cadena)
parser.printCounter
