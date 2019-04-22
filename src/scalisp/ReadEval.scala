package scalisp

import scala.util.parsing.combinator._

class Environment(enclosingEnvironment: Option[Environment]) {
  var bindings = Map[Symbol, Form]()

  def this() = {this(None)}

  def rootEnvironment(): Environment = {
    enclosingEnvironment match {
      case Some(e) => e.rootEnvironment()
      case _ => this
    }
  }

  def addBinding(s:Symbol, f:Form) = bindings = bindings + (s -> f)
  def getBinding(s:Symbol): Form = {
    //println("Getting binding for " + s + " in env " + this + " " + this.bindings + " " + enclosingEnvironment + " " +  rootEnvironment())
    if (bindings contains s)
      bindings(s)
    else if (enclosingEnvironment.isDefined)
      enclosingEnvironment.get.getBinding(s)
    else throw new Exception("Symbol " + s + " not defined in this context.")
  }
}

object Utilities {
  def isNil(f:Form): Boolean = f match {
    case Symbol("nil") => true
    case Symbol("false") => true
    case LispList(x) if x.isEmpty => true
    // Also boolean false, eventually.
    case _ => false
  }

  def lispCondition(b:Boolean) = {
    if (b)
      Symbol("t")
    else
      Symbol("false")
  }
}

/*  The ScaLisp AST */

// Anything which is meant to be evaluated is a form.
abstract class Form {
  def eval(env:Environment):Form
}
case class QuotedForm(form: Form) extends Form {
  override def toString() = form.toString()
  // Quotes forms evaluate to themselves.
  override def eval(env:Environment): Form = {
    form match {
      // The CLHS doesn't list this as a special case, but I'm not sure how it's handled otherwise.
      case LispList(forms :List[Form]) if forms.size == 0 => Symbol("nil")
      case _ => form
    }
  }
}
case class BackquotedForm(form: Form) extends Form {
  override def toString() = "`" + form.toString()

  // Recursively descend through the given form, evaluating the unquoted portions and substituting
  // the evaluated value back in to the form.
  def unquoteSubst(f: Form, env: Environment): Form = f match {
    // TODO: How do we deal with ``,,,a for instance?
    case uf: UnquotedForm => uf.eval(env)
    case LispList(forms :List[Form]) => LispList(forms.map(form => unquoteSubst(form, env)))
    case _ => f
  }

  override def eval(env:Environment): Form = {
    unquoteSubst(form, env)
  }
}
case class UnquotedForm(form: Form) extends Form {
  override def toString() = "," + form.toString()
  // Unquoted forms (those preceeded by , inside a `) evaluate to their value from the environment.
  override def eval(env:Environment): Form = {
    form.eval(env)
  }
}
case class Symbol(name: String) extends Form {
  override def toString() = name
  override def eval(env:Environment): Form = {
    env.getBinding(this)
  }
}
case class LispInteger(value: Integer) extends Form {
  override def toString() = value.toString
  override def eval(env:Environment): Form = return this // Integers evaluate to themselves.
  def +(other: LispInteger) = LispInteger(value + other.value)
  def -(other: LispInteger) = LispInteger(value - other.value)
  def *(other: LispInteger) = LispInteger(value * other.value)
  def /(other: LispInteger) = LispInteger(value / other.value)
}
case class LispList(args: List[Form]) extends Form {
  def tail(): Form = args match {
    case forms:List[Form] if forms.size <= 1 => Symbol("nil")
    case _ => LispList(args.tail)
  }

  override def toString() = "(" + args.mkString(" ") + ")"
  override def eval(env:Environment): Form = {
    args match {
      // Empty lists are nil
      case Nil => Symbol("nil")

      // Sometimes lists are special forms...
      // Starting with (some of) McCarthy's Elementary S-functions and Predicates (http://www-formal.stanford.edu/jmc/recursive.pdf)
      case List(Symbol("atom"), (f:Form)) => {
        val evaled = f.eval(env)
        Utilities.lispCondition(evaled.isInstanceOf[Symbol])
      }
      case List(Symbol("eq"), x:Form, y:Form) => {
        val evalx = x.eval(env)
        val evaly = y.eval(env)
        (evalx, evaly) match {
          case (Symbol(xs), Symbol(ys)) => Utilities.lispCondition(x == y)
          case _ => throw new Exception("eq is defined only for symbols.")
        }
      }

      // List operations
      case List(Symbol("car"), f:Form) => {
        val evaled = f.eval(env)
        evaled match {
          case LispList(l) => l.head
          case Symbol("nil") => Symbol("nil")
          case _ => throw new Exception("car is defined only for lists.")
        }
      }
      case List(Symbol("cdr"), f:Form) => {
        val evaled = f.eval(env)
        evaled match {
          case ll:LispList => ll.tail
          case Symbol("nil") => Symbol("nil")
          case _ => throw new Exception("cdr is defined only for lists.")
        }
      }
      case List(Symbol("cons"), car:Form, cdr:Form) => {
        val evalcar = car.eval(env)
        val evalcdr = cdr.eval(env)
        evalcdr match {
          case LispList(cdrlist) => LispList(evalcar :: cdrlist)
          case Symbol("nil") => LispList(List(evalcar))
          // Todo: This needs to go away when we add dotted cons.
          case _ => throw new Exception("cons requires a form and list.")
        }
      }

      // Math operations
      case Symbol("+") :: operands => {
        operands.map(o => o.eval(env)).asInstanceOf[List[LispInteger]].reduce((a,b) => a + b)
      }
      case Symbol("-") :: operands => {
        operands.map(o => o.eval(env)).asInstanceOf[List[LispInteger]].reduce((a,b) => a - b)
      }
      case Symbol("*") :: operands => {
        operands.map(o => o.eval(env)).asInstanceOf[List[LispInteger]].reduce((a,b) => a * b)
      }
      case Symbol("/") :: operands => {
        operands.map(o => o.eval(env)).asInstanceOf[List[LispInteger]].reduce((a,b) => a / b)
      }

      // Logic operations
      // These likely could be implemented as macros, but let's leave them here for now.
      // I'm sure there's also a more appropriate Scala way to do it too!
      case Symbol("or") :: operands => {
        for ( o:Form <- operands ) {
          val e = o.eval(env)
          if (!Utilities.isNil(e)) return e
        }
        Symbol("nil")
      }
      case Symbol("and") :: operands => {
        var e:Form = Symbol("t")
        for ( o:Form <- operands ) {
          e = o.eval(env)
          if (Utilities.isNil(e)) return Symbol("nil")
        }
        e
      }

      case List(Symbol("quote"), f:Form) => f

      case List(Symbol("if"), testform: Form, thenform: Form, elseform: Form) => Conditional(testform, thenform, Some(elseform)).eval(env)
      case List(Symbol("if"), testform: Form, thenform: Form) => Conditional(testform, thenform, None).eval(env)

      case Symbol("defun") :: Symbol(funname) :: LispList(params) :: body  => {
        val fun = Function(params.asInstanceOf[List[Symbol]], body)
        env.addBinding(Symbol(funname), fun)
        Symbol(funname)
      }
      case Symbol("lambda") :: LispList(params) :: body => {
        Function(params.asInstanceOf[List[Symbol]], body)
      }
      case Symbol("defmacro") :: Symbol(macroname) :: LispList(params) :: body => {
        // The body of a macro is expanded at compile time-time, but when handled in interpreted-mode (which is all
        // we plan to support), the expansion happens when the macro is called.
        val newmacro = Macro(params.asInstanceOf[List[Symbol]], body)
        env.addBinding(Symbol(macroname), newmacro);
        Symbol(macroname)
      }
      //      case List(Symbol("macroexpand"), f: Form) => {
      //        f match {
      //          case QuotedForm(m: Macro) => m.macroexpand()
      //        }
      //      }
      case List(Symbol("setf"), Symbol(varname), value:Form) => {
        val evaled = value.eval(env)
        // Setf happens at the root binding.
        env.rootEnvironment().addBinding(Symbol(varname), evaled)
        evaled
      }
      case Symbol("let") :: LispList(binds) :: body => {
        // The current environment is the parent of the letenv (unlike functions!)
        val letenv:Environment = new Environment(Some(env))
        // binds contains lists of 2 elements, where the first element is a name and the second is a value
        for (b:Form <- binds) {
          b match {
            case LispList(List(k: Symbol, v)) => letenv.addBinding(k, v.eval(env))
            case _ => throw new Exception("Let bindings require a name and value.")
          }
        }
        body match {
          case Nil => Symbol("nil")
          case _ => body.map(b => b.eval(letenv)).last
        }
      }



      // From CLHS: Note that an eval form involves two levels of evaluation for its argument.
      // First, form is evaluated by the normal argument evaluation mechanism as would occur with any call.
      // The object that results from this normal argument evaluation becomes the value of the form parameter,
      // and is then evaluated as part of the eval form.
      case List(Symbol("eval"), f:Form) => {
        val evaled = f.eval(env)
        evaled.eval(env)
      }

      // Sometimes lists call functions/macros...
      case Symbol(x) :: args => {
        val bind = env.getBinding(Symbol(x))
        bind match {
          // TODO: env needs an enclosing environment which is distinct from the calling environment (unless we want dynamic scope!)
          // How do we choose the environment? Let's go with the root environment for now.
          case call: Function => call.eval(call.bindParams(args, env, new Environment(Some(env.rootEnvironment()))))
          case call: Macro => call.eval(call.bindParams(args, env, new Environment(Some(env.rootEnvironment()))))
          case _ => throw new Exception("Undefined function " + x)
        }
      }
    }
  }
}
case class Function(params: List[Symbol], body: List[Form]) extends Form {
  def bindParams(args: List[Form], callingEnvironment: Environment, newEnvironment: Environment) = {
    // First ensure the binding is actually possible.
    if (params.size == args.size){
      // Evaluate the arguments in the calling environment, but add the results bound to the params
      // in the new environment.
      for ((p, a) <- params zip args) newEnvironment.addBinding(p, a.eval(callingEnvironment))
      newEnvironment
    }
    else throw new Exception("Incorrect number of arguments given to function.")
  }
  override def toString(): String = {
    "#<function lambda (" + params.mkString(" ") + ") " + body.mkString(" ") + ">"
  }
  override def eval(env:Environment): Form = body match {
    case Nil => Symbol("nil")
    case _ => (body.map(b => b.eval(env))).last
  }
}
case class Macro(params: List[Symbol], body: List[Form]) extends Form {
  def bindParams(args: List[Form], callingEnvironment: Environment, newEnvironment: Environment) = {
    // First ensure the binding is actually possible.
    if (params.size == args.size){
      // Don't evaluate the arguments, just let them get substituted in.
      for ((p, a) <- params zip args) newEnvironment.addBinding(p, a)
      newEnvironment
    }
    else throw new Exception("Incorrect number of arguments given to macro.")
  }
  override def toString(): String = {
    "#<macro (" + params.mkString(" ") + ") " + body.mkString(" ") + ">"
  }

  def macroexpand(env:Environment): Form ={
    body.map(b => b.eval(env)).last
    // I think all of this has been replaced by proper work in backquote evaluation.
    //    f match {
    //      case QuotedForm(form) =>
    //        if (backquoted) QuotedForm(macroexpand(form, env, backquoted))
    //        else form
    //      case UnquotedForm(form) =>
    //        if (backquoted) UnquotedForm(form).eval(env)
    //        else throw new Exception("Comma not allowed outside of backquote.") // TODO: Make this a read-time error.
    //      // TODO: This one might have problems. Do some looking into the spec further. (Is there a "stack" of quotes?)
    //      case BackquotedForm(form) => macroexpand(form, env, true)
    //      case LispList(forms :List[Form]) =>
    //        if (backquoted) LispList(forms.map(form => macroexpand(form, env, backquoted)))
    //        else f.eval(env)
    //      // Base case.
    //      case f: Form =>
    //        if (backquoted) f
    //        else f.eval(env)
    //    }
  }

  override def eval(env:Environment): Form = body match {
    case Nil => Symbol("nil")
    case _ => macroexpand(env).eval(env) // (body.map(b => macroexpand(b, env).eval(env))).last
  }
}
case class Conditional(testForm: Form, thenForm: Form, elseForm: Option[Form]) extends Form {
  override def toString() = elseForm match {
    case Some(x) => "(if " + testForm + " " + thenForm + " " + elseForm + ")"
    case _ => "(if " + testForm + " " + thenForm + ")"
  }
  override def eval(env:Environment): Form = {
    if (!Utilities.isNil(testForm.eval(env)))
      thenForm.eval(env)
    else {
      elseForm.getOrElse(Symbol("nil")).eval(env)
    }
  }
}

// nil n. the object that is at once the symbol named "NIL" in the COMMON-LISP package, the empty list, the boolean (or generalized boolean) representing false, and the name of the empty type.

// After evaluation, we create new structures for things like functions

/*
form = compound_form | atomic_symbol | self_evaluating_object

compound_form = list % Really, a function form.

self_evaluating_object = number

list = "(" s_expression < s_expression > ")"

conditional = "if" test-form then-form [else-form]

atomic_symbol = letter atom_part

atom_part = empty / letter atom_part / number atom_part

letter = "a" / "b" / " ..." / "z"

number = "1" / "2" / " ..." / "9"

empty = " "
*/

class LispParser extends RegexParsers {
  def form: Parser[Form] = self_evaluating_object | atomic_symbol | list | quoted_form
  def unquote_legal_form: Parser[Form] = self_evaluating_object | atomic_symbol | unquote_legal_list | unquoted_form | unquote_legal_quoted_form
  def unquoted_form: Parser[Form] = "," ~ unquote_legal_form ^^ {case _ ~ f => UnquotedForm(f)}
  def list: Parser[Form] = "(" ~ rep(form) ~ ")" ^^ { case _ ~ exps ~ _ => LispList(exps) }
  def unquote_legal_list: Parser[Form] = "(" ~ rep(unquote_legal_form) ~ ")" ^^ { case _ ~ exps ~ _ => LispList(exps) }
  def atomic_symbol: Parser[Form] = "[A-Za-z+\\-*/@$%^&_=<>~.][A-Za-z0-9+\\-*/@$%^&_=<>~.]*".r ^^ {str => Symbol(str)}
  def quoted_form: Parser[Form] = "'" ~ form ^^ {case _ ~ f => QuotedForm(f)} | backquoted_form
  def backquoted_form: Parser[Form] = "`" ~ unquote_legal_form ^^ {case _ ~ f => BackquotedForm(f)}
  def unquote_legal_quoted_form:Parser[Form] = "'" ~ unquote_legal_form ^^ {case _ ~ f => QuotedForm(f)} | backquoted_form
  def self_evaluating_object: Parser[Form] = integer
  def integer: Parser[Form] = "-?[0-9]+".r ^^ {num => LispInteger(Integer.valueOf(num))}
}

object ScaLisp {
  def main(args: Array[String]) {
    val lp = new LispParser()
    val exp2c: Form = lp.parseAll(lp.form, "(+ 1 2)").get
    println(exp2c)
  }
}
