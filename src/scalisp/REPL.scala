package scalisp

object REPL {
  def repl(): Unit ={
    var linectr = 1
    val lp = new LispParser()
    val rootEnv = new Environment()
    rootEnv.addBinding(Symbol("nil"), Symbol("nil"))
    rootEnv.addBinding(Symbol("t"), Symbol("t"))
    while(true) {
      // Read
      try {
        val input = scala.io.StdIn.readLine(s"[$linectr]> ")
        val readForm: Form = lp.parseAll(lp.form, input).get
        val evaledForm: Form = readForm.eval(rootEnv)
        print(evaledForm + "\n")
      }
      catch {
        // TODO: Refine exceptions.
        case e: Exception => println(e.getMessage)
      }
      linectr = linectr + 1
    }
  }

  def main(args: Array[String]){
    repl()
  }

}
