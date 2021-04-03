package ba.sake.flowrun
package eval

import scala.util._
import scala.concurrent.{ Future, Promise }
import concurrent.ExecutionContext.Implicits.global
import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.window
import ba.sake.flowrun.parse.Token

class Interpreter(programModel: ProgramModel) {
  import Interpreter._

  private val symTab = SymbolTable()

  private var state = State.INITIALIZED

  def run(): Unit = {
    import js.JSConverters._
    //pprint.pprintln(programModel.ast)
    state = State.RUNNING

    // run statements sequentually
    // start from empty future, 
    // wait for it -> then next, next...
    // https://users.scala-lang.org/t/process-a-list-future-sequentially/3704/4
    val statements = programModel.ast.statements
    val futureExec = statements.foldLeft(Future.unit){ (a, b) =>
      a.flatMap(_ => interpret(b))
    }

    futureExec.onComplete { _ =>
      state = State.FINISHED
    }
    futureExec.onComplete {
      case Success(_) =>
      case Failure(e: EvalException) =>
        state = State.FAILED
        EventUtils.dispatchEvent("eval-error",
          js.Dynamic.literal(
            msg = e.getMessage,
            nodeId = e.nodeId
          )
        )
      case Failure(e) =>
        println(s"Unexpected error: $e")
    }
  }

  def continue(): Unit =
    state = State.RUNNING

  private def interpret(stmt: Statement): Future[Unit] = waitForContinue().map { _ =>
    println(s"interpreting: $stmt")
    import Statement._
    
    stmt match {
      case Declare(id, name, tpe, initValue) =>
        if (name.trim.isEmpty)
          throw EvalException(s"Not a valid name: '$name'", id)
        val maybeExprVal = initValue.map(e => eval(id, e))
        symTab.add(name, tpe, Symbol.Kind.Var, maybeExprVal)
      case Assign(id, name, expr) =>
        val exprValue = eval(id, expr)
        symTab.set(id, name, exprValue)
      case Input(id, name, value) =>
        state = State.PAUSED
      case Output(id, expr) =>
        val outputValue = eval(id, expr)
        val newOutput = Option(outputValue).getOrElse("null").toString
        EventUtils.dispatchEvent("eval-output",
          js.Dynamic.literal(
            output = newOutput
          )
        )
      case If(id, condition, ifTrueStatements, ifFalseStatements) =>
        val condValue = eval(id, condition)
        condValue match {
          case condition: Boolean =>
            if (condition) interpret(ifTrueStatements)
            else interpret(ifFalseStatements)
          case _ => throw EvalException(s"Not a valid condition: '$condValue'", id)
        }
      case block: Block =>
        block.statements.foreach(interpret)
      case Begin | End | BlockEnd(_) => // noop
    }
    ()
  }

  private def eval(id: String, expr: Expression): Any =
    var tmp1 = eval(id, expr.boolOrComparison)
    if !tmp1.isInstanceOf[Boolean] then return tmp1
    var tmp = tmp1.asInstanceOf[Boolean]
    expr.boolOrComparisons.foreach { nextBoolOrOpt =>
      val nextVal = eval(id, nextBoolOrOpt.boolAndComparison).asInstanceOf[Boolean]
      tmp = tmp || nextVal
    }
    tmp

  private def eval(id: String, boolOrComparison: BoolOrComparison): Any =
    var tmp1 = eval(id, boolOrComparison.boolAndComparison)
    if !tmp1.isInstanceOf[Boolean] then return tmp1
    var tmp = tmp1.asInstanceOf[Boolean]
    boolOrComparison.boolAndComparisons.foreach { nextBoolAndOpt =>
      val nextVal = eval(id, nextBoolAndOpt.numComparison).asInstanceOf[Boolean]
      tmp = tmp && nextVal
    }
    tmp

  private def eval(id: String, boolAndComparison: BoolAndComparison): Any =
    var tmp1 = eval(id, boolAndComparison.numComparison)
    if !tmp1.isInstanceOf[Boolean] then return tmp1
    var tmp = tmp1.asInstanceOf[Boolean]

    boolAndComparison.numComparisons.foreach { nextNumCompOpt =>
      val nextVal = eval(id, nextNumCompOpt.numComparison).asInstanceOf[Boolean]
      nextNumCompOpt.op.tpe match
        case Token.Type.EqualsEquals  => tmp = tmp == nextVal
        case _                        => tmp = tmp != nextVal
    }
    tmp

  private def eval(id: String, numComparison: NumComparison): Any =
    var tmp1 = eval(id, numComparison.term)
    if !tmp1.isInstanceOf[Double] then return tmp1
    var tmp = tmp1.asInstanceOf[Double]

    // TODO if ima VIŠE TERMOVA, nema smisla: 5>7>8
    numComparison.terms.headOption match
      case Some(nextTermOpt) =>
        val nextVal = eval(id, nextTermOpt.term).asInstanceOf[Double]
        nextTermOpt.op.tpe match
          case Token.Type.Lt    => tmp < nextVal
          case Token.Type.LtEq  => tmp <= nextVal
          case Token.Type.Gt    => tmp > nextVal
          case _                => tmp >= nextVal
      case None => tmp

  private def eval(id: String, term: Term): Any =
    var tmp1 = eval(id, term.factor)
    val isNum = tmp1.isInstanceOf[Double]
    val isString = tmp1.isInstanceOf[String]
    if isNum then
      var tmp = tmp1.asInstanceOf[Double]
      term.factors.foreach { nextFactorOpt =>
        val nextVal = eval(id, nextFactorOpt.factor).asInstanceOf[Double] // TODO
        nextFactorOpt.op.tpe match
          case Token.Type.Plus  => tmp += nextVal
          case _                => tmp -= nextVal
      }
      tmp
    else if isString then
      var tmp = tmp1.asInstanceOf[String]
      term.factors.foreach { nextFactorOpt =>
        val nextVal = eval(id, nextFactorOpt.factor).toString
        nextFactorOpt.op.tpe match
          case Token.Type.Plus  => tmp += nextVal
          case _                => throw EvalException("Cannot subtract Strings", id)
      }
      tmp
    else tmp1

  private def eval(id: String, factor: Factor): Any =
    var tmp1 = eval(id, factor.unary)
    if !tmp1.isInstanceOf[Double] then return tmp1
    var tmp = tmp1.asInstanceOf[Double]
    factor.unaries.foreach { nextUnaryOpt =>
      val nextVal = eval(id, nextUnaryOpt.unary).asInstanceOf[Double]
      nextUnaryOpt.op.tpe match
        case Token.Type.Times  => tmp *= nextVal
        case Token.Type.Div    => tmp /= nextVal
        case _                 => tmp %= nextVal
    }
    tmp

  private def eval(id: String, unary: Unary): Any =
    import Unary._
    unary match
      case Prefixed(op, unary) =>
        val next = eval(id, unary)
        if op.tpe == Token.Type.Minus
        then -next.asInstanceOf[Double]
        else !next.asInstanceOf[Boolean]
      case Simple(atom)        => eval(id, atom)

  private def eval(id: String, atom: Atom): Any =
    import Atom._
    atom match
      case NumberLit(value)   => value
      case StringLit(value)   => value
      case Identifier(name)   => symTab.get(id, name)
      case TrueLit            => true
      case FalseLit           => false
      case NullLit            => null
      case Parens(expression) => eval(id, expression)
    
  
  // adapted https://stackoverflow.com/a/46619347/4496364
  private def waitForContinue(): Future[Unit] = {
    val p = Promise[Unit]()
    val pingHandle: js.timers.SetIntervalHandle = js.timers.setInterval(50) {
      println("STATE: " + state)
      if state == State.RUNNING && !p.isCompleted then
        p.success(())
        
    }
    val f = p.future
    f.onComplete { _ => 
      js.timers.clearInterval(pingHandle)
    }
    f
  }
}

object Interpreter:
  enum State:
    case INITIALIZED, RUNNING, PAUSED, FINISHED, FAILED