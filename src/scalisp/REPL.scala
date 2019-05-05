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
        val inputForms = replReadString(linectr)
        val readForms: List[Form] = inputForms.map(i => lp.parseAll(lp.form, i).get)
        for (readForm: Form <- readForms){
          val evaledForm: Form = readForm.eval(rootEnv)
          print(evaledForm + "\n")
        }
      }
      catch {
        // TODO: Refine exceptions.
        case e: Exception => println(e.getMessage)
      }
      linectr = linectr + 1
    }
  }

  def replReadString(linectr: Int): List[String] = {
    var forms: List[String] = List()
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
          if (count == 0) {
            forms = forms :+ form
            form = ""
          }
        }
      }
      if (count == 0){
        if (form != "") forms = forms :+ form
        return forms
      }
      form += ' '
      input = scala.io.StdIn.readLine(s"      ")
    }
    return List()
  }

  def main(args: Array[String]){
    repl()
  }

}
