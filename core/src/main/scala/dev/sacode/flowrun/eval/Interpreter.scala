package dev.sacode.flowrun
package eval

import scala.util.*
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scalajs.js
import reactify.*
import dev.sacode.flowrun.parse.{Token, parseExpr, ParseException, LexException}

class Interpreter(programModel: ProgramModel, flowrunChannel: Channel[FlowRun.Event]) {
  import Interpreter.*

  val symTab = SymbolTable(flowrunChannel)

  private var state = State.INITIALIZED

  // TODO add predefined functions also.. :)
  // more general solution I guess...
  private def allFunctions = List(programModel.ast.main) ++ programModel.ast.functions

  def run(): Future[Unit] = {
    //import js.JSConverters.*
    //pprint.pprintln(programModel.ast)
    println(programModel.ast.toJson)

    state = State.RUNNING

    val futureDeclareFuncs = Future.successful {
      allFunctions.foreach { fun =>
        val key = SymbolKey(fun.name, Symbol.Kind.Function)
        symTab.add(null, key, fun.tpe, None)
      }
    }

    val futureExec = futureDeclareFuncs.flatMap { _ =>
      interpret(programModel.ast.main, List.empty)
    }

    futureExec.onComplete {
      case Success(_) =>
        state = State.FINISHED
        flowrunChannel := FlowRun.Event.EvalSuccess
      case Failure(e: EvalException) =>
        state = State.FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage)
      case Failure(e: ParseException) =>
        state = State.FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage)
      case Failure(e: LexException) =>
        state = State.FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage)
      case Failure(e) =>
        println(s"Unexpected error: $e")
    }
    futureExec.map(_ => ())
  }

  def continue(): Unit =
    state = State.RUNNING

  private def interpret(
      fun: Function,
      arguments: List[(String, Expression.Type, Any)]
  ): Future[Any] =
    symTab.enterScope(fun.name)
    arguments.foreach { (name, tpe, value) =>
      val key = SymbolKey(name, Symbol.Kind.Variable)
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
        if name.trim.isEmpty then throw EvalException(s"Not a valid name: '$name'", id)

        // TODO validate reserved identifiers
        // TODO validate [a-zA-Z][a-zA-Z0-9]*
        if !name.trim.matches("[a-zA-Z][a-zA-Z0-9]*") then
          throw EvalException(s"Not a valid name: '$name'", id)

        val maybeInitValueExpr = initValue.map(iv => parseExpr(id, iv))
        maybeInitValueExpr match
          case None =>
            val key = SymbolKey(name, Symbol.Kind.Variable)
            symTab.add(id, key, tpe, None)
            Future.successful({})
          case Some(expr) =>
            eval(id, expr).map { v =>
              TypeUtils.getUpdateValue(id, name, tpe, v).get // validate
              val key = SymbolKey(name, Symbol.Kind.Variable)
              symTab.add(id, key, tpe, Some(v))
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
        eval(id, parseExpr(id, expr)).map(_ => ())
      case Input(id, name) =>
        if !symTab.isDeclaredVar(name) then
          throw EvalException(s"Variable '$name' is not declared.", id)
        state = State.PAUSED
        flowrunChannel := FlowRun.Event.EvalInput(id, name)
        waitForContinue()
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
      case While(id, condition, body) =>
        def loop(): Future[Any] =
          eval(id, parseExpr(id, condition)).flatMap {
            case condition: Boolean =>
              if (condition) interpret(body).flatMap(_ => loop())
              else Future.successful({})
            case condValue => throw EvalException(s"Not a valid condition: '$condValue'", id)
          }
        loop()
      case DoWhile(id, condition, body) =>
        def loop(): Future[Any] =
          eval(id, parseExpr(id, condition)).flatMap {
            case condition: Boolean =>
              if (condition) interpret(body).flatMap(_ => loop())
              else Future.successful({})
            case condValue => throw EvalException(s"Not a valid condition: '$condValue'", id)
          }
        loop()
      case block: Block =>
        execSequentially((): Any, block.statements, (_, s) => interpret(s))
      case Return(id, maybeExpr) => // noop
        maybeExpr match
          case None       => Future.successful(())
          case Some(expr) => eval(id, parseExpr(id, expr))
      case Dummy(_) => // noop
        Future.successful(())
      case Begin | End => // noop
        Future.successful(())
    }
  }

  private def eval(id: String, expr: Expression): Future[Any] =
    eval(id, expr.boolOrComparison).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Boolean] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Boolean]
        execSequentially(
          first,
          expr.boolOrComparisons,
          (acc, nextBoolOrOpt) => {
            eval(id, nextBoolOrOpt.boolAndComparison).map { v =>
              val nextVal = v.asInstanceOf[Boolean]
              acc || nextVal
            }
          }
        )
    }

  private def eval(id: String, boolOrComparison: BoolOrComparison): Future[Any] =
    eval(id, boolOrComparison.boolAndComparison).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Boolean] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Boolean]
        execSequentially(
          first,
          boolOrComparison.boolAndComparisons,
          (acc, nextBoolAndOpt) => {
            eval(id, nextBoolAndOpt.numComparison).map { v =>
              val nextVal = v.asInstanceOf[Boolean]
              acc && nextVal
            }
          }
        )
    }

  private def eval(id: String, boolAndComparison: BoolAndComparison): Future[Any] =
    eval(id, boolAndComparison.numComparison).flatMap { first =>
      execSequentially(
        first,
        boolAndComparison.numComparisons,
        (acc, nextNumCompOpt) => {
          eval(id, nextNumCompOpt.numComparison).map { nextVal =>
            nextNumCompOpt.op.tpe match
              case Token.Type.Plus => acc == nextVal
              case _               => acc != nextVal
          }
        }
      )
    }

  private def eval(id: String, numComparison: NumComparison): Future[Any] =
    eval(id, numComparison.term).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Double] then Future.successful(tmp1)
      else
        val tmp = tmp1.asInstanceOf[Double]
        numComparison.terms match
          case Some(nextTermOpt) =>
            eval(id, nextTermOpt.term).map { v =>
              val nextVal = v.asInstanceOf[Double]
              nextTermOpt.op.tpe match
                case Token.Type.Lt   => tmp < nextVal
                case Token.Type.LtEq => tmp <= nextVal
                case Token.Type.Gt   => tmp > nextVal
                case _               => tmp >= nextVal
            }
          case None => Future.successful(tmp)
    }

  private def eval(id: String, term: Term): Future[Any] =
    eval(id, term.factor).flatMap { tmp1 =>
      val isNum = tmp1.isInstanceOf[Double]
      val isString = tmp1.isInstanceOf[String]
      if isNum then
        val first = tmp1.asInstanceOf[Double]
        execSequentially(
          first,
          term.factors,
          (acc, nextFactorOpt) => {
            eval(id, nextFactorOpt.factor).map { v =>
              val nextVal = v.asInstanceOf[Double] // TODO Integer
              nextFactorOpt.op.tpe match
                case Token.Type.Plus => acc + nextVal
                case _               => acc - nextVal
            }
          }
        )
      else if isString then
        val first = tmp1.asInstanceOf[String]
        execSequentially(
          first,
          term.factors,
          (acc, nextFactorOpt) => {
            eval(id, nextFactorOpt.factor).map { v =>
              val nextVal = v.toString
              nextFactorOpt.op.tpe match
                case Token.Type.Plus => acc + nextVal
                case _               => throw EvalException("Cannot subtract Strings", id)
            }
          }
        )
      else Future.successful(tmp1)
    }

  private def eval(id: String, factor: Factor): Future[Any] =
    eval(id, factor.unary).flatMap { tmp1 =>
      if !tmp1.isInstanceOf[Double] then Future.successful(tmp1)
      else
        val first = tmp1.asInstanceOf[Double]
        execSequentially(
          first,
          factor.unaries,
          (acc, nextUnaryOpt) => {
            eval(id, nextUnaryOpt.unary).map { v =>
              val nextVal = v.asInstanceOf[Double]
              nextUnaryOpt.op.tpe match
                case Token.Type.Times => acc * nextVal
                case Token.Type.Div   => acc / nextVal
                case _                => acc % nextVal
            }
          }
        )
    }

  private def eval(id: String, unary: Unary): Future[Any] =
    unary match
      case Unary.Prefixed(op, unary) =>
        eval(id, unary).map { next =>
          if op.tpe == Token.Type.Minus then -next.asInstanceOf[Double]
          else !next.asInstanceOf[Boolean]
        }
      case Unary.Simple(atom) => eval(id, atom)

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
        val key = SymbolKey(name, Symbol.Kind.Function)
        val funSym = symTab.getSymbol(id, key) // check if defined

        val futureArgs = execSequentially(
          List.empty,
          argumentExprs,
          (acc, nextExpr) => {
            eval(id, nextExpr).map(arg => acc.appended(arg))
          }
        )

        futureArgs.flatMap { args =>
          if name == "abs" then
            // TODO handle all predefined functions
            // TODO validate args..........
            Future.successful(Math.abs(args.head.asInstanceOf[Double]))
          else
            val fun = allFunctions.find(_.name == name).get
            if args.size != fun.parameters.size then
              throw EvalException(
                s"Wrong number of parameters. Expected: ${fun.parameters.size}, got ${args.size}",
                id
              )
            val argsWithTypes = args.zip(fun.parameters).zipWithIndex.map {
              case ((arg, (paramName, paramTpe)), idx) =>
                // validate expected type
                if TypeUtils.getUpdateValue(id, paramName, paramTpe, arg).isFailure then
                  throw EvalException(
                    s"Expected: '${paramName}: ${paramTpe}' at index $idx, got value '$arg'",
                    id
                  )
                (paramName, paramTpe, arg)
            }
            interpret(fun, argsWithTypes)
        }

  // adapted https://stackoverflow.com/a/46619347/4496364
  private def waitForContinue(): Future[Unit] = {
    val p = Promise[Unit]()
    val pingHandle: js.timers.SetIntervalHandle = js.timers.setInterval(10) {
      if state == State.RUNNING && !p.isCompleted then p.success(())
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
    case INITIALIZED, RUNNING, PAUSED, FINISHED, FAILED
