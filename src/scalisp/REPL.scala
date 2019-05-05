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
        val input = replReadString(linectr)
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

  def replReadString(linectr: Int): String = {
    var form = ""
    var count = 0
    var input = scala.io.StdIn.readLine(s"[$linectr]> ")
    while(true) {
      for (c: Char <- input) {
        form += c
        if (c == '(') {
          count += 1
        }
        else if (c == ')') {
          count -= 1
          if (count == 0) return form
        }
      }
      if (count == 0) return form
      form += ' '
      input = scala.io.StdIn.readLine(s"      ")
    }
    return ""
  }

  def main(args: Array[String]){
    repl()
  }

}
