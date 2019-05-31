package scalisp

import java.io.File

import scala.io._

object REPL {
  def repl(): Unit ={
    var linectr = 1
    val lp = new LispParser()
    val rootEnv = new Environment()
    rootEnv.addBinding(Symbol("nil"), Symbol("nil"))
    rootEnv.addBinding(Symbol("t"), Symbol("t"))

    // Load builtins file
    val inputForms = fileReadString(new File("./src/scalisp/builtin.lisp"))
    val readForms: List[Form] = inputForms.map(i => lp.parseAll(lp.form, i).get)
    for (readForm: Form <- readForms){
      readForm.eval(rootEnv)
    }

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

  // TODO: We may want to generalize all of this on top of InputStream or some Scala equivalent.
  def replReadString(linectr: Int): List[String] = {
    var forms: List[String] = List()
    var form = ""
    var count = 0
    var input = scala.io.StdIn.readLine(s"[$linectr]> ")
    while(true) {
      var comment = false
      for (c: Char <- input) {
        if (c == ';') comment = true
        if (!comment) {
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
      }
      if (count == 0){
        if (form.trim() != "") forms = forms :+ form
        return forms
      }
      form += ' '
      input = scala.io.StdIn.readLine(s"      ")
    }
    return List()
  }

  // This is intended for the built-in functions.
  // TODO: If we want generalized file reading/writing we should implement with-open-file and load.
  def fileReadString(file: File): List[String] = {
    val bufferedSource = Source.fromFile(file)

    var forms: List[String] = List()
    var form = ""
    var count = 0

    for (line <- bufferedSource.getLines) {
      var comment = false
      for (c: Char <- line) {
        if (c == ';') comment = true
        if (!comment) {
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
      }
      if (count == 0){
        if (form.trim() != "") forms = forms :+ form
      }
      form += ' '
    }

    bufferedSource.close

    if (count != 0) throw new Exception("EOF while reading file.")

    return forms
  }

  def main(args: Array[String]){
    repl()
  }

}
