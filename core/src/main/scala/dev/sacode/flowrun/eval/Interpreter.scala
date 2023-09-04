package dev.sacode.flowrun.eval

import scala.util.*
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scalajs.js
import reactify.*
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.parse.{Token, parseExpr, ParseException, LexException}
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.FlowRun
import RunVal.*

final class Interpreter(programModel: ProgramModel, flowrunChannel: Channel[FlowRun.Event]) {
  import Interpreter.*

  val symTab = SymbolTable(flowrunChannel)

  var state = State.INITIALIZED

  def run(): Future[Unit] = {

    state = State.RUNNING

    val functionsFuture = Future { // Future needed coz symTab throws
      programModel.ast.allFunctions.foreach { fun =>
        symTab.addFun(fun.id, fun.name, fun.tpe, None)
      }
    }

    // main is also just-a-function, with its own scope
    val futureExec = for
      _ <- functionsFuture
      res <- interpretFunction(programModel.ast.main, List.empty)
    yield res

    futureExec.onComplete {
      case Success(_) =>
        state = State.FINISHED_SUCCESS
        flowrunChannel := FlowRun.Event.EvalSuccess
      case Failure(e: EvalException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage, symTab.currentScope.id)
      case Failure(e: ParseException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage, symTab.currentScope.id)
      case Failure(e: LexException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(e.nodeId, e.getMessage, symTab.currentScope.id)
      case Failure(e: StoppedException) =>
        state = State.FINISHED_STOPPED
        flowrunChannel := FlowRun.Event.EvalError("", e.getMessage, symTab.currentScope.id)
      case Failure(e) =>
        state = State.FINISHED_FAILED
        // this can be any JS failure, that's why we don't print it to user
        println(s"Unexpected error: $e")
    }

    futureExec.map(_ => {})
  }

  def setValue(nodeId: String, name: String, inputValue: String): Option[RunVal] =
    val sym = symTab.getSymbolVar(nodeId, name)
    try {
      val value = sym.tpe match
        case Type.Integer => IntegerVal(inputValue.toInt)
        case Type.Real    => RealVal(inputValue.toDouble)
        case Type.Boolean => BooleanVal(inputValue.toBoolean)
        case Type.String  => StringVal(inputValue)
        case Type.Void    => throw EvalException("Cant set Void var... (should not happen)", nodeId)
      symTab.setValue(nodeId, name, value)
      state = State.RUNNING
      Some(value)
    } catch {
      case (e: EvalException) => // from symbol table
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(nodeId, e.getMessage, symTab.currentScope.id)
        None
      case e: (NumberFormatException | IllegalArgumentException) =>
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(
          nodeId,
          s"You entered invalid ${sym.tpe}: ${inputValue}",
          symTab.currentScope.id
        )
        None
    }

  private def interpretFunction(
      fun: Function,
      arguments: List[(String, Type, RunVal)]
  ): Future[RunVal] =
    symTab.enterScope(fun.id, fun.name)
    flowrunChannel := FlowRun.Event.FunctionEntered(fun)
    arguments.foreach { (name, tpe, value) =>
      symTab.addVar(fun.id, name, tpe, Some(value))
    }
    execSequentially(NoVal, fun.statements, (_, s) => interpretStatement(s)).map { result =>
      symTab.exitScope()
      flowrunChannel := FlowRun.Event.FunctionEntered(fun)
      result
    }

  private def interpretStatement(stmt: Statement): Future[RunVal] = waitForContinue().flatMap { _ =>
    // println(s"interpreting: $stmt")
    import Statement.*

    stmt match {

      case Declare(id, name, tpe, initValue) =>
        val maybeInitValueExpr = initValue.map(iv => parseExpr(id, iv))
        maybeInitValueExpr match
          case None =>
            Future {
              symTab.addVar(id, name, tpe, None)
              NoVal
            }
          case Some(expr) =>
            evalExpr(id, expr).map { v =>
              val promotedVal = v.promote(id, name, tpe)
              symTab.addVar(id, name, tpe, Some(promotedVal))
              NoVal
            }

      case Assign(id, name, expr) =>
        if !symTab.isDeclaredVar(name) then throw EvalException(s"Variable '$name' is not declared.", id)
        val sym = symTab.getSymbolVar(id, name)
        evalExpr(id, parseExpr(id, expr)).map { exprValue =>
          if exprValue.valueOpt.get.toString.isEmpty && sym.tpe != Type.String then
            throw EvalException(s"Assign expression cannot be empty.", id)
          val promotedVal = exprValue.promote(id, name, sym.tpe)

          symTab.setValue(id, name, promotedVal)
          NoVal
        }

      case Call(id, expr) =>
        evalExpr(id, parseExpr(id, expr)).map(_ => NoVal)

      case Input(id, name, prompt) =>
        if !symTab.isDeclaredVar(name) then throw EvalException(s"Variable '$name' is not declared.", id)
        state = State.PAUSED
        flowrunChannel := FlowRun.Event.EvalInput(id, name, prompt)
        waitForContinue().map(_ => NoVal)

      case Output(id, expr, newline) =>
        evalExpr(id, parseExpr(id, expr)).map { outputValue =>
          val newOutput = outputValue.valueOpt.getOrElse("null").toString
          flowrunChannel := FlowRun.Event.EvalOutput(newOutput, newline)
          NoVal
        }

      case If(id, condition, ifTrueStatements, ifFalseStatements) =>
        evalExpr(id, parseExpr(id, condition)).flatMap {
          case condition: BooleanVal =>
            if (condition.value) interpretStatement(ifTrueStatements)
            else interpretStatement(ifFalseStatements)
          case condValue => throw EvalException(s"Not a valid condition: '${condValue.valueOpt.getOrElse("")}'", id)
        }

      case While(id, condition, body) =>
        def loop(): Future[RunVal] =
          evalExpr(id, parseExpr(id, condition)).flatMap {
            case condition: BooleanVal =>
              if (condition.value) interpretStatement(body).flatMap(_ => loop())
              else Future.successful(NoVal)
            case condValue => throw EvalException(s"Not a valid condition: '${condValue.valueOpt.getOrElse("")}'", id)
          }
        loop()

      case DoWhile(id, condition, body) =>
        def loop(): Future[RunVal] =
          evalExpr(id, parseExpr(id, condition)).flatMap {
            case condition: BooleanVal =>
              if (condition.value) interpretStatement(body).flatMap(_ => loop())
              else Future.successful(NoVal)
            case condValue => throw EvalException(s"Not a valid condition: '${condValue.valueOpt.getOrElse("")}'", id)
          }
        interpretStatement(body).flatMap(_ => loop())

      case ForLoop(id, varName, startExpr, incrExpr, endExpr, body) =>
        def loop(conditionExpr: String, incr: Int): Future[RunVal] =
          evalExpr(id, parseExpr(id, conditionExpr)).flatMap {
            case condition: BooleanVal =>
              if (condition.value) interpretStatement(body).flatMap { _ =>
                val current = symTab.getValue(id, varName).asInstanceOf[IntegerVal]
                symTab.setValue(id, varName, current.transform(_ + incr))
                loop(conditionExpr, incr)
              }
              else Future.successful(NoVal)
            case condValue => throw EvalException(s"Not a valid condition: '${condValue.valueOpt.getOrElse("")}'", id)
          }

        for {
          startAny <- evalExpr(id, parseExpr(id, startExpr))
          incrAny <- evalExpr(id, parseExpr(id, incrExpr))
          endAny <- evalExpr(id, parseExpr(id, endExpr))

          start = startAny.asInstanceOf[IntegerVal]
          incr = incrAny.asInstanceOf[IntegerVal]
          end = endAny.asInstanceOf[IntegerVal]

          // maybe declare a new var
          _ =
            if symTab.isDeclaredVar(varName) then symTab.setValue(id, varName, start)
            else symTab.addVar(id, varName, Type.Integer, Some(start))

          comparator = if incr.value >= 0 then "<=" else ">="
          conditionExpr = s"$varName $comparator ${end.value}"
          _ <- loop(conditionExpr, incr.value)
        } yield NoVal

      case block: Block =>
        execSequentially(NoVal, block.statements, (_, s) => interpretStatement(s))

      case Return(id, maybeExpr) =>
        val retValFut = maybeExpr match
          case None       => Future.successful(NoVal)
          case Some(expr) => evalExpr(id, parseExpr(id, expr))
        retValFut.map { retVal =>
          val currentExecFun = programModel.ast.allFunctions.find(_.id == symTab.currentScope.id).get
          if retVal.tpe != currentExecFun.tpe then
            throw EvalException(
              s"Expected function '${currentExecFun.name}' to return '${currentExecFun.tpe}' but got '${retVal.pretty}'",
              id
            )
          else retVal
        }

      case Begin(_) =>
        Future.successful(NoVal)
    }
  }

  private def evalExpr(id: String, expr: Expression): Future[RunVal] =
    evalBoolOrComparison(id, expr.boolOrComparison).flatMap {
      case boolVal: BooleanVal =>
        execSequentially(
          boolVal,
          expr.boolOrComparisons,
          (acc, nextBoolOrOpt) => {
            if acc.value then Future.successful(BooleanVal(true)) // short circuit when TRUE
            else
              evalBoolAndComparison(id, nextBoolOrOpt.boolAndComparison).map {
                case nextVal: BooleanVal => acc.transform(_ || nextVal.value)
                case otherVal =>
                  throw EvalException(
                    s"Expected a Boolean but got '${otherVal.pretty}' while evaluating || operation.",
                    id
                  )
              }
          }
        )
      case otherVal => Future.successful(otherVal)
    }

  private def evalBoolOrComparison(id: String, boolOrComparison: BoolOrComparison): Future[RunVal] =
    evalBoolAndComparison(id, boolOrComparison.boolAndComparison).flatMap {
      case boolVal: BooleanVal =>
        execSequentially(
          boolVal,
          boolOrComparison.boolAndComparisons,
          (acc, nextBoolAndOpt) => {
            if !acc.value then Future.successful(BooleanVal(false)) // short circuit when FALSE
            else
              evalNumComparison(id, nextBoolAndOpt.numComparison).map {
                case nextVal: BooleanVal => acc.transform(_ && nextVal.value)
                case otherVal =>
                  throw EvalException(
                    s"Expected a Boolean but got '${otherVal.pretty}' while evaluating && operation.",
                    id
                  )
              }
          }
        )
      case otherVal => Future.successful(otherVal)
    }

  private def evalBoolAndComparison(id: String, boolAndComparison: BoolAndComparison): Future[RunVal] =
    evalNumComparison(id, boolAndComparison.numComparison).flatMap { first =>
      execSequentially(
        first,
        boolAndComparison.numComparisons,
        (acc, nextNumCompOpt) => {
          val isEquals = nextNumCompOpt.op.tpe == Token.Type.EqualsEquals
          evalNumComparison(id, nextNumCompOpt.numComparison).map { nextVal =>
            (acc, nextVal) match
              case (v1: IntegerVal, v2: IntegerVal) =>
                if isEquals then BooleanVal(v1.value == v2.value) else BooleanVal(v1.value != v2.value)
              case (v1: RealVal, v2: RealVal) =>
                if isEquals then BooleanVal(v1.value == v2.value) else BooleanVal(v1.value != v2.value)
              case (v1: RealVal, v2: IntegerVal) => // promote Integer to Real
                if isEquals then BooleanVal(v1.value == v2.value.toDouble)
                else BooleanVal(v1.value != v2.value.toDouble)
              case (v1: IntegerVal, v2: RealVal) => // promote Integer to Real
                if isEquals then BooleanVal(v1.value.toDouble == v2.value)
                else BooleanVal(v1.value.toDouble != v2.value)
              case (v1: StringVal, v2: StringVal) =>
                if isEquals then BooleanVal(v1.value == v2.value) else BooleanVal(v1.value != v2.value)
              case (v1: BooleanVal, v2: BooleanVal) =>
                if isEquals then BooleanVal(v1.value == v2.value) else BooleanVal(v1.value != v2.value)
              case (v1, v2) =>
                throw EvalException(
                  s"Values '${v1.pretty}' and '${v2.pretty}' are not comparable.",
                  id
                )
          }
        }
      )
    }

  private def evalNumComparison(id: String, numComparison: NumComparison): Future[RunVal] =
    evalTerm(id, numComparison.term).flatMap {
      case numVal: (IntegerVal | RealVal) =>
        numComparison.terms match
          case Some(nextTermOpt) =>
            evalTerm(id, nextTermOpt.term).map { nextVal =>
              // just promote both to Double and done
              val v1: Double = numVal.promote(id, "", Type.Real).asInstanceOf[RealVal].value
              val v2: Double = nextVal.promote(id, "", Type.Real).asInstanceOf[RealVal].value
              nextTermOpt.op.tpe match
                case Token.Type.Lt   => BooleanVal(v1 < v2)
                case Token.Type.LtEq => BooleanVal(v1 <= v2)
                case Token.Type.Gt   => BooleanVal(v1 > v2)
                case _               => BooleanVal(v1 >= v2)
            }
          case None => Future.successful(numVal)
      case otherVal => Future.successful(otherVal)
    }

  private def evalTerm(id: String, term: Term): Future[RunVal] =
    evalFactor(id, term.factor).flatMap {
      case numVal: (IntegerVal | RealVal) =>
        execSequentially(
          numVal,
          term.factors,
          (acc, nextFactorOpt) => {
            val isPlus = nextFactorOpt.op.tpe == Token.Type.Plus
            evalFactor(id, nextFactorOpt.factor).map { nextVal =>
              (acc, nextVal) match
                case (v1: IntegerVal, v2: IntegerVal) =>
                  if isPlus then IntegerVal(v1.value + v2.value) else IntegerVal(v1.value - v2.value)
                case (v1: RealVal, v2: RealVal) =>
                  if isPlus then RealVal(v1.value + v2.value) else RealVal(v1.value - v2.value)
                case (v1: RealVal, v2: IntegerVal) => // promote Integer to Real
                  if isPlus then RealVal(v1.value + v2.value.toDouble) else RealVal(v1.value - v2.value.toDouble)
                case (v1: IntegerVal, v2: RealVal) => // promote Integer to Real
                  if isPlus then RealVal(v1.value.toDouble + v2.value) else RealVal(v1.value.toDouble - v2.value)
                case (v1, v2) =>
                  val op = if isPlus then "sum" else "deduct"
                  throw EvalException(
                    s"Cannot $op '${v1.pretty}' and '${v2.pretty}'",
                    id
                  )
            }
          }
        )
      case stringVal: StringVal =>
        execSequentially(
          stringVal,
          term.factors,
          (acc, nextFactorOpt) => {
            evalFactor(id, nextFactorOpt.factor).map { v =>
              val nextVal = v.valueOpt.map(_.toString).getOrElse("")
              nextFactorOpt.op.tpe match
                case Token.Type.Plus => acc.transform(_ + nextVal)
                case _               => throw EvalException("Cannot subtract Strings", id)
            }
          }
        )
      case otherVal => Future.successful(otherVal)
    }

  private def evalFactor(id: String, factor: Factor): Future[RunVal] =
    evalUnary(id, factor.unary).flatMap {
      case numVal: (IntegerVal | RealVal) =>
        execSequentially(
          numVal,
          factor.unaries,
          (acc, nextUnaryOpt) => {
            val isTimes = nextUnaryOpt.op.tpe == Token.Type.Times
            val isDiv = nextUnaryOpt.op.tpe == Token.Type.Div
            evalUnary(id, nextUnaryOpt.unary).map { nextVal =>
              (acc, nextVal) match
                case (v1: IntegerVal, v2: IntegerVal) =>
                  if isTimes then IntegerVal(v1.value * v2.value)
                  else if isDiv then IntegerVal(v1.value / v2.value)
                  else IntegerVal(v1.value % v2.value)
                case (v1: RealVal, v2: RealVal) =>
                  if isTimes then RealVal(v1.value * v2.value)
                  else if isDiv then RealVal(v1.value / v2.value)
                  else RealVal(v1.value % v2.value)
                case (v1: RealVal, v2: IntegerVal) => // promote Integer to Real
                  if isTimes then RealVal(v1.value * v2.value.toDouble)
                  else if isDiv then RealVal(v1.value / v2.value.toDouble)
                  else RealVal(v1.value % v2.value.toDouble)
                case (v1: IntegerVal, v2: RealVal) => // promote Integer to Real
                  if isTimes then RealVal(v1.value.toDouble * v2.value)
                  else if isDiv then RealVal(v1.value.toDouble / v2.value)
                  else RealVal(v1.value.toDouble % v2.value)
                case (v1, v2) =>
                  val op = if isTimes then "multiply" else if isDiv then "divide" else "mod"
                  throw EvalException(
                    s"Cannot $op '${v1.pretty}' and '${v2.pretty}'",
                    id
                  )
            }
          }
        )
      case otherVal => Future.successful(otherVal)
    }

  private def evalUnary(id: String, unary: Unary): Future[RunVal] =
    unary match
      case Unary.Prefixed(op, unary) =>
        evalUnary(id, unary).map { next =>
          if op.tpe == Token.Type.Minus then
            next match
              case n: IntegerVal => n.transform(v => -v)
              case n: RealVal    => n.transform(v => -v)
              case _             => throw EvalException(s"Cant negate '${next.pretty}'", id)
          else next.asInstanceOf[BooleanVal].transform(v => !v)
        }
      case Unary.Simple(atom) => evalAtom(id, atom)

  private def evalAtom(id: String, atom: Atom): Future[RunVal] =
    import Atom.*
    atom match
      case IntegerLit(value)  => Future.successful(IntegerVal(value))
      case RealLit(value)     => Future.successful(RealVal(value))
      case StringLit(value)   => Future.successful(StringVal(value))
      case Identifier(name)   => Future(symTab.getValue(id, name)) // can throw, that's why not Future.successful
      case TrueLit            => Future.successful(BooleanVal(true))
      case FalseLit           => Future.successful(BooleanVal(false))
      case Parens(expression) => evalExpr(id, expression)
      case FunctionCall(name, argumentExprs) =>
        val futureArgs = execSequentially(
          List.empty[RunVal],
          argumentExprs,
          (acc, nextExpr) => {
            evalExpr(id, nextExpr).map(arg => acc.appended(arg))
          }
        )

        futureArgs.flatMap { args =>
          PredefinedFunction.withName(name) match
            case Some(f) =>
              handlePredefinedFunction(id, f, args)
            case None =>
              val funSym = symTab.getSymbolFun(id, name)
              val fun = programModel.ast.allFunctions.find(_.name == name).get
              validateArgsNumber(id, fun.name, fun.parameters.size, args.size)
              val argsWithTypes = args.zip(fun.parameters).zipWithIndex.map { case ((arg, p), idx) =>
                if arg.tpe != p.tpe then
                  throw EvalException(
                    s"Expected: '${p.pretty}' at index $idx, got value '${arg.pretty}'",
                    id
                  )
                (p.name, p.tpe, arg)
              }
              interpretFunction(fun, argsWithTypes)
        }

  // adapted https://stackoverflow.com/a/46619347/4496364
  private def waitForContinue(): Future[Unit] = {
    // TODO if program running TOOOOOOO LONG
    val p = Promise[Unit]()
    val pingHandle: js.timers.SetIntervalHandle = js.timers.setInterval(10) {
      if !p.isCompleted then {
        if state == State.RUNNING then p.success({})
        else if state == State.FINISHED_STOPPED then p.failure(StoppedException("Program stopped"))
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

  private def handlePredefinedFunction(id: String, f: PredefinedFunction, args: Seq[RunVal]): Future[RunVal] =
    import PredefinedFunction.*
    f match {
      // numbers
      case func @ Abs =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(n.transform(_.abs))
          case n: RealVal    => Future(n.transform(_.abs)) // polymorphic, overloaded..
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Floor =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: RealVal => Future(n.transform(_.floor))
          case _          => throw EvalException(s"Expected a Real argument in function ${func.name}", id)
      case func @ Ceil =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: RealVal => Future(n.transform(_.ceil))
          case _          => throw EvalException(s"Expected a Real argument in function ${func.name}", id)
      case func @ RandomInteger =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(n.transform(x => Random.nextInt(x)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Sin =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.sin(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(x => Math.sin(x)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Cos =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.cos(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(x => Math.cos(x)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Tan =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.tan(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(x => Math.tan(x)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Ln =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.log(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(x => Math.log(x)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Log10 =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.log10(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(x => Math.log10(x)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Log2 =>
        validateArgsNumber(id, func.name, 1, args.size)

        args.head match
          case n: IntegerVal => Future(RealVal(Math.log(n.value) / Math.log(2)))
          case n: RealVal    => Future(n.transform(x => Math.log(x) / Math.log(2)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      // strings
      case func @ Length =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case s: StringVal => Future(IntegerVal(s.value.length))
          case _            => throw EvalException(s"Expected a String argument in function ${func.name}", id)
      case func @ CharAt =>
        validateArgsNumber(id, func.name, 2, args.size)
        val str = args.head
        val idx = args(1)
        (str, idx) match
          case (s: StringVal, i: IntegerVal) => Future(s.transform(_.apply(i.value).toString))
          case _ => throw EvalException(s"Expected (String, Integer) arguments in function ${func.name}", id)
      // conversions
      case func @ RealToInteger =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: RealVal => Future(IntegerVal(n.value.toInt))
          case _          => throw EvalException(s"Expected a Real argument in function ${func.name}", id)

          // conversions
      case func @ StringToInteger =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: StringVal => Future(IntegerVal(n.value.toInt))
          case _          => throw EvalException(s"Expected a String argument in function ${func.name}", id)
    }

  private def validateArgsNumber(id: String, funName: String, expected: Int, got: Int): Unit =
    if got != expected then
      throw EvalException(s"Wrong number of arguments in function $funName, expected $expected but got $got", id)
}

object Interpreter:
  enum State:
    case INITIALIZED, RUNNING, PAUSED, FINISHED_SUCCESS, FINISHED_STOPPED, FINISHED_FAILED
