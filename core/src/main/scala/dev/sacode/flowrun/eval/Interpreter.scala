package dev.sacode.flowrun
package eval

import scala.util.*
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scalajs.js
import reactify.*
import dev.sacode.flowrun.parse.{Token, parseExpr, ParseException, LexException}

/*
- at runtime we use Int and Double
  we "widen" literals like 5.0 to 5.000000000000000000001 bcoz IDK better
 */
class Interpreter(programModel: ProgramModel, flowrunChannel: Channel[FlowRun.Event]) {
  import Interpreter.*

  val symTab = SymbolTable(flowrunChannel)

  var state = State.INITIALIZED

  // TODO add predefined functions also.. :)
  // more general solution I guess...
  private def allFunctions = programModel.ast.functions

  def run(): Future[Unit] = {

    state = State.RUNNING

    val functionsFuture = Future { // needed coz SymbolKey throws
      allFunctions.foreach { fun =>
        val key = SymbolKey(fun.name, Symbol.Kind.Function, "")
        symTab.add(null, key, fun.tpe, None)
      }
    }

    // main is also just-a-function, with its own scope
    val futureExec = for
      _ <- functionsFuture
      res <- interpret(programModel.ast.main, List.empty)
    yield res

    futureExec.onComplete {
      case Success(_) =>
        state = State.FINISHED_SUCCESS
        flowrunChannel := FlowRun.Event.EvalSuccess
      case Failure(e: EvalException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage)
      case Failure(e: ParseException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage)
      case Failure(e: LexException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage)
      case Failure(e) =>
        state = State.FINISHED_FAILED
        // this can be any JS failure, that's why we dont't print it to user
        println(s"Unexpected error: $e")
    }

    futureExec.mapTo[Unit]
  }

  def setValue(nodeId: String, name: String, inputValue: String): Option[(Expression.Type, Any)] =
    val key = SymbolKey(name, Symbol.Kind.Variable, nodeId)
    val sym = symTab.getSymbol(nodeId, key)
    try {
      val value = sym.tpe match
        case Expression.Type.Integer => inputValue.toInteger
        case Expression.Type.Real    => inputValue.toReal
        case Expression.Type.Boolean => inputValue.toBoolean
        case Expression.Type.String  => inputValue
        case Expression.Type.Void    => null
      symTab.setValue(nodeId, name, value)
      state = State.RUNNING
      Some((sym.tpe, value))
    } catch {
      case (e: EvalException) => // from symbol table
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(nodeId, e.getMessage)
        None
      case e: (NumberFormatException | IllegalArgumentException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(nodeId, s"You entered invalid ${sym.tpe}: ${inputValue}")
        None
    }

  private def interpret(
      fun: Function,
      arguments: List[(String, Expression.Type, Any)]
  ): Future[Any] =
    symTab.enterScope(fun.name)
    arguments.foreach { (name, tpe, value) =>
      val key = SymbolKey(name, Symbol.Kind.Variable, "")
      symTab.add(null, key, tpe, Some(value))
    }
    execSequentially((): Any, fun.statements, (_, s) => interpret(s)).map { result =>
      symTab.exitScope()
      result
    }

  private def interpret(stmt: Statement): Future[Any] = waitForContinue().flatMap { _ =>
    //println(s"interpreting: $stmt")
    import Statement.*

    stmt match {

      case Declare(id, name, tpe, initValue) =>
        val maybeInitValueExpr = initValue.map(iv => parseExpr(id, iv))
        maybeInitValueExpr match
          case None =>
            Future {
              val key = SymbolKey(name, Symbol.Kind.Variable, id)
              symTab.add(id, key, tpe, None)
            }
          case Some(expr) =>
            evalExpr(id, expr).map { v =>
              TypeUtils.getValue(id, tpe, v).get // validate
              val key = SymbolKey(name, Symbol.Kind.Variable, id)
              symTab.add(id, key, tpe, Some(v))
              ()
            }

      case Assign(id, name, expr) =>
        if !symTab.isDeclaredVar(name) then throw EvalException(s"Variable '$name' is not declared.", id)
        val key = SymbolKey(name, Symbol.Kind.Variable, id)
        val sym = symTab.getSymbol(id, key)
        evalExpr(id, parseExpr(id, expr)).map { exprValue =>
          if exprValue.toString.isEmpty && sym.tpe != Expression.Type.String then
            throw EvalException(s"Assign expression cannot be empty.", id)
          symTab.setValue(id, name, exprValue)
        }

      case Call(id, expr) =>
        evalExpr(id, parseExpr(id, expr)).map(_ => ())

      case Input(id, name) =>
        if !symTab.isDeclaredVar(name) then throw EvalException(s"Variable '$name' is not declared.", id)
        state = State.PAUSED
        flowrunChannel := FlowRun.Event.EvalInput(id, name)
        waitForContinue()

      case Output(id, expr) =>
        evalExpr(id, parseExpr(id, expr)).map { outputValue =>
          val newOutput = Option(outputValue).getOrElse("null").toString
          flowrunChannel := FlowRun.Event.EvalOutput(newOutput)
          ()
        }

      case If(id, condition, ifTrueStatements, ifFalseStatements) =>
        evalExpr(id, parseExpr(id, condition)).flatMap {
          case condition: Boolean =>
            if (condition) interpret(ifTrueStatements)
            else interpret(ifFalseStatements)
          case condValue => throw EvalException(s"Not a valid condition: '$condValue'", id)
        }

      case While(id, condition, body) =>
        def loop(): Future[Any] =
          evalExpr(id, parseExpr(id, condition)).flatMap {
            case condition: Boolean =>
              if (condition) interpret(body).flatMap(_ => loop())
              else Future.successful({})
            case condValue => throw EvalException(s"Not a valid condition: '$condValue'", id)
          }
        loop()

      case DoWhile(id, condition, body) =>
        def loop(): Future[Any] =
          evalExpr(id, parseExpr(id, condition)).flatMap {
            case condition: Boolean =>
              if (condition) interpret(body).flatMap(_ => loop())
              else Future.successful({})
            case condValue => throw EvalException(s"Not a valid condition: '$condValue'", id)
          }
        interpret(body).flatMap(_ => loop())

      case ForLoop(id, varName, startExpr, incrExpr, endExpr, body) =>
        def loop(conditionExpr: String, incr: Int): Future[Any] =
          evalExpr(id, parseExpr(id, conditionExpr)).flatMap {
            case condition: Boolean =>
              if (condition) interpret(body).flatMap { _ =>
                val current = symTab.getValue(id, varName)
                symTab.setValue(id, varName, current.toInteger + incr)
                loop(conditionExpr, incr)
              }
              else Future.successful({})
            case condValue => throw EvalException(s"Not a valid condition: '$condValue'", id)
          }

        for {
          startAny <- evalExpr(id, parseExpr(id, startExpr))
          incrAny <- evalExpr(id, parseExpr(id, incrExpr))
          endAny <- evalExpr(id, parseExpr(id, endExpr))

          start = startAny.toInteger
          incr = incrAny.toInteger
          end = endAny.toInteger

          // maybe declare a new var
          _ =
            if symTab.isDeclaredVar(varName) then symTab.setValue(id, varName, start)
            else
              val key = SymbolKey(varName, Symbol.Kind.Variable, id)
              symTab.add(id, key, Expression.Type.Integer, Some(start))

          comparator = if incr >= 0 then "<=" else ">="
          conditionExpr = s"$varName $comparator $end"
          _ <- loop(conditionExpr, incr)
        } yield ()

      case block: Block =>
        execSequentially((): Any, block.statements, (_, s) => interpret(s))

      case Return(id, maybeExpr) =>
        maybeExpr match
          case None       => Future.successful(())
          case Some(expr) => evalExpr(id, parseExpr(id, expr))

      case Begin(_) => // noop
        Future.successful({})
    }
  }

  private def evalExpr(id: String, expr: Expression): Future[Any] =
    evalBoolOrComparison(id, expr.boolOrComparison).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Boolean] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Boolean]
        execSequentially(
          first,
          expr.boolOrComparisons,
          (acc, nextBoolOrOpt) => {
            evalBoolAndComparison(id, nextBoolOrOpt.boolAndComparison).map { v =>
              TypeUtils.getValue(id, Expression.Type.Boolean, v).get // validate
              val nextVal = v.asInstanceOf[Boolean]
              acc || nextVal
            }
          }
        )
    }

  private def evalBoolOrComparison(id: String, boolOrComparison: BoolOrComparison): Future[Any] =
    evalBoolAndComparison(id, boolOrComparison.boolAndComparison).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Boolean] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Boolean]
        execSequentially(
          first,
          boolOrComparison.boolAndComparisons,
          (acc, nextBoolAndOpt) => {
            evalNumComparison(id, nextBoolAndOpt.numComparison).map { v =>
              TypeUtils.getValue(id, Expression.Type.Boolean, v).get // validate
              val nextVal = v.asInstanceOf[Boolean]
              acc && nextVal
            }
          }
        )
    }

  private def evalBoolAndComparison(id: String, boolAndComparison: BoolAndComparison): Future[Any] =
    evalNumComparison(id, boolAndComparison.numComparison).flatMap { first =>
      execSequentially(
        first,
        boolAndComparison.numComparisons,
        (acc, nextNumCompOpt) => {
          evalNumComparison(id, nextNumCompOpt.numComparison).map { nextVal =>
            nextNumCompOpt.op.tpe match
              case Token.Type.EqualsEquals => acc == nextVal
              case _                       => acc != nextVal
          }
        }
      )
    }

  private def evalNumComparison(id: String, numComparison: NumComparison): Future[Any] =
    evalTerm(id, numComparison.term).flatMap { tmp1 =>
      if tmp1.isInstanceOf[Int] || tmp1.isInstanceOf[Double] then
        val tmp = tmp1.toReal
        numComparison.terms match
          case Some(nextTermOpt) =>
            evalTerm(id, nextTermOpt.term).map { v =>
              val nextVal = v.toReal
              nextTermOpt.op.tpe match
                case Token.Type.Lt   => tmp < nextVal
                case Token.Type.LtEq => tmp <= nextVal
                case Token.Type.Gt   => tmp > nextVal
                case _               => tmp >= nextVal
            }
          case None => Future.successful(tmp)
      else Future.successful(tmp1)
    }

  private def evalTerm(id: String, term: Term): Future[Any] =
    evalFactor(id, term.factor).flatMap { tmp1 =>
      if tmp1.isInstanceOf[Int] || tmp1.isInstanceOf[Double] then
        execSequentially(
          tmp1.toReal,
          term.factors,
          (acc, nextFactorOpt) => {
            evalFactor(id, nextFactorOpt.factor).map { v =>
              val nextVal = v.toReal
              nextFactorOpt.op.tpe match
                case Token.Type.Plus => acc + nextVal
                case _               => acc - nextVal
            }
          }
        )
      else if tmp1.isInstanceOf[String] then
        execSequentially(
          tmp1.toString,
          term.factors,
          (acc, nextFactorOpt) => {
            evalFactor(id, nextFactorOpt.factor).map { v =>
              val nextVal = v.toString
              nextFactorOpt.op.tpe match
                case Token.Type.Plus => acc + nextVal
                case _               => throw EvalException("Cannot subtract Strings", id)
            }
          }
        )
      else Future.successful(tmp1)
    }

  private def evalFactor(id: String, factor: Factor): Future[Any] =
    evalUnary(id, factor.unary).flatMap { tmp1 =>
      if tmp1.isInstanceOf[Int] then
        execSequentially(
          tmp1.toInteger,
          factor.unaries,
          (acc, nextUnaryOpt) => {
            evalUnary(id, nextUnaryOpt.unary).map { v =>
              TypeUtils.getValue(id, Expression.Type.Integer, v).get // validate
              val nextVal = v.toInteger
              nextUnaryOpt.op.tpe match
                case Token.Type.Times => acc * nextVal
                case Token.Type.Div   => acc / nextVal
                case _                => acc % nextVal
            }
          }
        )
      else if tmp1.isInstanceOf[Double] then
        execSequentially(
          tmp1.toReal,
          factor.unaries,
          (acc, nextUnaryOpt) => {
            evalUnary(id, nextUnaryOpt.unary).map { v =>
              TypeUtils.getValue(id, Expression.Type.Real, v).getOrElse {
                TypeUtils.getValue(id, Expression.Type.Integer, v).get
              } // validate
              val nextVal = v.toReal
              nextUnaryOpt.op.tpe match
                case Token.Type.Times => acc * nextVal
                case Token.Type.Div   => acc / nextVal
                case _                => acc % nextVal
            }
          }
        )
      else Future.successful(tmp1)
    }

  private def evalUnary(id: String, unary: Unary): Future[Any] =
    unary match
      case Unary.Prefixed(op, unary) =>
        evalUnary(id, unary).map { next =>
          if op.tpe == Token.Type.Minus then
            if next.isInstanceOf[Int] then -next.toInteger
            else -next.toReal
          else !next.asInstanceOf[Boolean]
        }
      case Unary.Simple(atom) => evalAtom(id, atom)

  private def evalAtom(id: String, atom: Atom): Future[Any] =
    import Atom.*
    atom match
      case IntegerLit(value) =>
        Future.successful(value)
      case RealLit(value) =>
        // add just a tinyyyyyyy bit to diferrentiate integer from double in runtime
        val res = if value.isValidInt then {
          value + 1e-308
        } else value
        Future.successful[Double](res)
      case StringLit(value)   => Future.successful(value)
      case Identifier(name)   => Future(symTab.getValue(id, name))
      case TrueLit            => Future.successful(true)
      case FalseLit           => Future.successful(false)
      case Parens(expression) => evalExpr(id, expression)
      case FunctionCall(name, argumentExprs) =>
        val key = SymbolKey(name, Symbol.Kind.Function, id)
        val funSym = symTab.getSymbol(id, key) // check if defined

        val futureArgs = execSequentially(
          List.empty,
          argumentExprs,
          (acc, nextExpr) => {
            evalExpr(id, nextExpr).map(arg => acc.appended(arg))
          }
        )

        futureArgs.flatMap { args =>
          if name == "abs" then
            // TODO handle all predefined functions
            // TODO validate args..........
            Future(Math.abs(args.head.toReal))
          else
            val fun = allFunctions.find(_.name == name).get
            if args.size != fun.parameters.size then
              throw EvalException(
                s"Wrong number of arguments, expected: ${fun.parameters.size} but got ${args.size}",
                id
              )
            val argsWithTypes = args.zip(fun.parameters).zipWithIndex.map { case ((arg, p), idx) =>
              // validate expected type
              if TypeUtils.getValue(id, p.tpe, arg).isFailure then
                throw EvalException(
                  s"Expected: '${p.name}: ${p.tpe}' at index $idx, got value '$arg'",
                  id
                )
              (p.name, p.tpe, arg)
            }
            interpret(fun, argsWithTypes)
        }

  // adapted https://stackoverflow.com/a/46619347/4496364
  private def waitForContinue(): Future[Unit] = {
    val p = Promise[Unit]()
    val pingHandle: js.timers.SetIntervalHandle = js.timers.setInterval(10) {
      if !p.isCompleted then {
        if state == State.RUNNING || state == State.FINISHED_STOPPED then p.success(())
      }
    }
    val f = p.future
    f.onComplete { _ =>
      js.timers.clearInterval(pingHandle)
    }
    f
  }

  // Run futures sequentually, starting with init.
  // Start from Future.successful(init),
  // wait for it -> then next, then next...
  // https://users.scala-lang.org/t/process-a-list-future-sequentially/3704/4
  private def execSequentially[T, Res](
      init: Res,
      values: List[T],
      f: (Res, T) => Future[Res]
  ): Future[Res] =
    val initF = Future.successful(init)
    values.foldLeft(initF) { (a, next) =>
      a.flatMap(acc => f(acc, next))
    }
}

object Interpreter:
  enum State:
    case INITIALIZED, RUNNING, PAUSED, FINISHED_SUCCESS, FINISHED_STOPPED, FINISHED_FAILED
