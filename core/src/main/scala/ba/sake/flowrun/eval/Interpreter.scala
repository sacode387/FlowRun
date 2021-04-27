package ba.sake.flowrun
package eval

import scala.util.*
import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global
import scalajs.js
import reactify.*
import ba.sake.flowrun.parse.{ Token, parseExpr }

class Interpreter(programModel: ProgramModel, flowrunChannel: Channel[FlowRun.Event]) {
  import Interpreter.*

  val symTab = SymbolTable(flowrunChannel)

  private var state = State.INITIALIZED

  private def allFunctions = List(programModel.ast.main) ++ programModel.ast.functions

  def run(): Future[Unit] = {
    //import js.JSConverters.*
    //pprint.pprintln(programModel.ast)
    println(js.JSON.stringify(programModel.ast.toNative))

    state = State.RUNNING

    val futureValidateFuncs = Future {
      allFunctions.foreach { fun =>
        val key = SymbolKey(fun.name, Symbol.Kind.Function)
        symTab.add(null, key, fun.tpe, None)
      }
    }

    val futureExec = futureValidateFuncs.flatMap { _ =>
      interpret(programModel.ast.main)
    }

    futureExec.onComplete {
      case Success(_) =>
        state = State.FINISHED
      case Failure(e: EvalException) =>
        state = State.FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage)
      case Failure(e) =>
        println(s"Unexpected error: $e")
    }
    futureExec
  }

  def continue(): Unit =
    state = State.RUNNING

  private def interpret(fun: Function): Future[Unit] =
    symTab.enterScope(fun.name)
    execSequentially((), fun.statements, (_, s) => interpret(s)).map { _ =>
      symTab.exitScope()
    }

  private def interpret(stmt: Statement): Future[Unit] = waitForContinue().flatMap { _ =>
    //println(s"interpreting: $stmt")
    import Statement.*
    
    stmt match {
      case Declare(id, name, tpe, initValue) =>
        if name.trim.isEmpty then
          throw EvalException(s"Not a valid name: '$name'", id)
        val maybeInitValueExpr = initValue.map(iv => parseExpr(id, iv))
        maybeInitValueExpr match
          case None =>
            val key = SymbolKey(name, Symbol.Kind.Variable)
            symTab.add(id, key, Some(tpe), None)
            Future.successful(())
          case Some(expr) =>
            eval(id, expr).map { v =>
              TypeUtils.getUpdateValue(id, name, tpe, v) // validate
              val key = SymbolKey(name, Symbol.Kind.Variable)
              symTab.add(id, key, Some(tpe), Some(v))
              ()
            }
      case Assign(id, name, expr) =>
        if !symTab.isDeclaredVar(name) then
          throw EvalException(s"Variable '$name' is not declared.", id)
        val key = SymbolKey(name, Symbol.Kind.Variable)
        val sym = symTab.getSymbol(id, key)
        eval(id, parseExpr(id, expr)).map { exprValue =>
          if exprValue.toString.isEmpty && sym.tpe != Expression.Type.String then
            throw EvalException(s"Assign expression cannot be empty.", id)
          symTab.setValue(id, name, exprValue)
        }
      case Call(id, expr) =>
        eval(id, parseExpr(id, expr)).map(_ =>())
      case Input(id, name) =>
        if !symTab.isDeclaredVar(name) then
          throw EvalException(s"Variable '$name' is not declared.", id)
        state = State.PAUSED
        flowrunChannel := FlowRun.Event.EvalInput(id, name)
        Future.successful(())
      case Output(id, expr) =>
        eval(id, parseExpr(id, expr)).map { outputValue =>
          val newOutput = Option(outputValue).getOrElse("null").toString
          flowrunChannel := FlowRun.Event.EvalOutput(newOutput)
          ()
        }
      case If(id, condition, ifTrueStatements, ifFalseStatements) =>
        eval(id, parseExpr(id, condition)).flatMap {
          case condition: Boolean =>
            if (condition) interpret(ifTrueStatements)
            else interpret(ifFalseStatements)
          case condValue => throw EvalException(s"Not a valid condition: '$condValue'", id)
        }
      case block: Block =>
        execSequentially((), block.statements, (_, s) => interpret(s))
      case Begin | End | BlockEnd(_) | Dummy(_) => // noop
        Future.successful(())
    }
  }

  private def eval(id: String, expr: Expression): Future[Any] =
    eval(id, expr.boolOrComparison).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Boolean] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Boolean]
        execSequentially(first, expr.boolOrComparisons, (acc, nextBoolOrOpt) => {
          eval(id, nextBoolOrOpt.boolAndComparison).map { v =>
            val nextVal = v.asInstanceOf[Boolean]
            acc || nextVal
          }
        })
    }

  private def eval(id: String, boolOrComparison: BoolOrComparison): Future[Any] =
    eval(id, boolOrComparison.boolAndComparison).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Boolean] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Boolean]
        execSequentially(first, boolOrComparison.boolAndComparisons, (acc, nextBoolAndOpt) => {
          eval(id, nextBoolAndOpt.numComparison).map { v =>
            val nextVal = v.asInstanceOf[Boolean]
            acc && nextVal
          }
        })
    }

  private def eval(id: String, boolAndComparison: BoolAndComparison): Future[Any] =
    eval(id, boolAndComparison.numComparison).flatMap { first =>
      execSequentially(first, boolAndComparison.numComparisons, (acc, nextNumCompOpt) => {
        eval(id, nextNumCompOpt.numComparison).map { nextVal =>
          nextNumCompOpt.op.tpe match
            case Token.Type.Plus => acc == nextVal
            case _               => acc != nextVal
        }
      })
    }

  private def eval(id: String, numComparison: NumComparison): Future[Any] =
    eval(id, numComparison.term).flatMap { tmp1 =>
      // TODO if ima VIŠE TERMOVA THROWWWWWW, nema smisla: 5>7>8
      if !tmp1.isInstanceOf[Double] then Future.successful(tmp1)
      else
        val tmp = tmp1.asInstanceOf[Double]
        numComparison.terms.headOption match
        case Some(nextTermOpt) =>
          eval(id, nextTermOpt.term).map { v =>
            val nextVal = v.asInstanceOf[Double]
            nextTermOpt.op.tpe match
            case Token.Type.Lt    => tmp < nextVal
            case Token.Type.LtEq  => tmp <= nextVal
            case Token.Type.Gt    => tmp > nextVal
            case _                => tmp >= nextVal
        }
        case None => Future.successful(tmp)
    }

  private def eval(id: String, term: Term): Future[Any] =
    eval(id, term.factor).flatMap { tmp1 =>
      val isNum = tmp1.isInstanceOf[Double]
      val isString = tmp1.isInstanceOf[String]
      if isNum then
        val first = tmp1.asInstanceOf[Double]
        execSequentially(first, term.factors, (acc, nextFactorOpt) => {
          eval(id, nextFactorOpt.factor).map { v =>
            val nextVal = v.asInstanceOf[Double] // TODO Integer
            nextFactorOpt.op.tpe match
              case Token.Type.Plus => acc + nextVal
              case _               => acc - nextVal
          }
        })
      else if isString then
        val first = tmp1.asInstanceOf[String]
        execSequentially(first, term.factors, (acc, nextFactorOpt) => {
          eval(id, nextFactorOpt.factor).map { v =>
            val nextVal = v.toString
            nextFactorOpt.op.tpe match
              case Token.Type.Plus => acc + nextVal
              case _               => throw EvalException("Cannot subtract Strings", id)
          }
        })
      else Future.successful(tmp1)
    }

  private def eval(id: String, factor: Factor): Future[Any] =
    eval(id, factor.unary).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Double] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Double]
        execSequentially(first, factor.unaries, (acc, nextUnaryOpt) => {
          eval(id, nextUnaryOpt.unary).map { v =>
            val nextVal = v.asInstanceOf[Double]
            nextUnaryOpt.op.tpe match
              case Token.Type.Times  => acc * nextVal
              case Token.Type.Div    => acc / nextVal
              case _                 => acc % nextVal
          }
        })
    }

  private def eval(id: String, unary: Unary): Future[Any] =
    unary match
      case Unary.Prefixed(op, unary) =>
        eval(id, unary).map { next =>
          if op.tpe == Token.Type.Minus
          then -next.asInstanceOf[Double]
          else !next.asInstanceOf[Boolean]
        }
      case Unary.Simple(atom)        => eval(id, atom)

  private def eval(id: String, atom: Atom): Future[Any] =
    import Atom.*
    atom match
      case NumberLit(value)   => Future.successful(value)
      case StringLit(value)   => Future.successful(value)
      case Identifier(name)   => Future.successful(symTab.getValue(id, name))
      case TrueLit            => Future.successful(true)
      case FalseLit           => Future.successful(false)
      case Parens(expression) => eval(id, expression)
      case FunctionCall(name, argumentExprs) =>
        // TODO handle predefined functions: abs, sin, cos...
        val key = SymbolKey(name, Symbol.Kind.Function)
        val funSym = symTab.getSymbol(id, key) // check if defined
        val fun = allFunctions.find(_.name == name).get
        interpret(fun)
  
  // adapted https://stackoverflow.com/a/46619347/4496364
  private def waitForContinue(): Future[Unit] = {
    val p = Promise[Unit]()
    val pingHandle: js.timers.SetIntervalHandle = js.timers.setInterval(10) {
      //println("STATE: " + state)
      if state == State.RUNNING && !p.isCompleted then
        p.success(())
    }
    val f = p.future
    f.onComplete { _ => 
      js.timers.clearInterval(pingHandle)
    }
    f
  }

  // run futures sequentually, starting with init.
  // Start from Future.successful(init),
  // wait for it -> then next, then next...
  // https://users.scala-lang.org/t/process-a-list-future-sequentially/3704/4
  private def execSequentially[T, Res](init: Res, values: List[T], f: (Res, T) => Future[Res]): Future[Res] =
    val initF = Future.successful(init)
    values.foldLeft(initF) { case (a, next) =>
      a.flatMap(acc => f(acc, next))
    }
}

object Interpreter:
  enum State:
    case INITIALIZED, RUNNING, PAUSED, FINISHED, FAILED