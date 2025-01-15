package dev.sacode.flowrun.eval

import scala.util.*
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import reactify.*
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.parse.{Token, parseExpr, ParseException, LexException}
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.FlowRun
import RunVal.*
import dev.sacode.flowrun.ast.Statement.Begin
import dev.sacode.flowrun.ast.Statement.Return
import dev.sacode.flowrun.ast.Statement.Declare
import dev.sacode.flowrun.ast.Statement.Assign
import dev.sacode.flowrun.ast.Statement.Call
import dev.sacode.flowrun.ast.Statement.Input
import dev.sacode.flowrun.ast.Statement.Output
import dev.sacode.flowrun.ast.Statement.Block
import dev.sacode.flowrun.ast.Statement.If
import dev.sacode.flowrun.ast.Statement.While
import dev.sacode.flowrun.ast.Statement.DoWhile
import dev.sacode.flowrun.ast.Statement.ForLoop
import dev.sacode.flowrun.ast.Statement.Comment

final class Interpreter(
    val programModel: ProgramModel,
    flowrunChannel: Channel[FlowRun.Event],
    setInterval: (Long, => Unit) => Any,
    clearInterval: Any => Unit
) {
  import Interpreter.*

  val symTab: SymbolTable = SymbolTable(flowrunChannel)

  var state: State = State.INITIALIZED

  // when user clicks step_by_step button
  // exec next statement and again set this to false
  var stepNext: Boolean = false

  var execMode: ExecMode = ExecMode.NORMAL

  var currentExecFunctionId: Option[String] = None
  var nextExecStatementId: Option[String] = None

  // input from user
  private var lastReadInput: Option[RunVal] = None

  def isRunning: Boolean = state == State.RUNNING || state == State.WAITING_FOR_INPUT

  def run(): Future[Unit] = run(ExecMode.NORMAL)

  def run(execMode: ExecMode): Future[Unit] = {

    this.execMode = execMode
    if execMode == ExecMode.STEP_BY_STEP then this.stepNext = true

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

  // almost same as setValue, but without any variable name
  def setLastReadInput(nodeId: String, inputValue: String): RunVal =
    val value = RunVal.fromString(inputValue)
    lastReadInput = Some(value)
    state = State.RUNNING
    value

  def setValue(nodeId: String, name: String, inputValue: String): Option[RunVal] =
    val sym = symTab.getSymbolVar(nodeId, name)
    try {
      val value = sym.tpe match
        case Type.Integer => IntegerVal(inputValue.toInt)
        case Type.Real    => RealVal(inputValue.toDouble)
        case Type.Boolean => BooleanVal(inputValue.toBoolean)
        case Type.String  => StringVal(inputValue)
        case tpe          => throw EvalException(s"Input variables of type ${tpe} are not supported.", nodeId)
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
    currentExecFunctionId = Some(fun.id)
    symTab.enterScope(fun.id, fun.name)
    flowrunChannel := FlowRun.Event.EvalFunctionStarted
    arguments.foreach { (name, tpe, value) =>
      symTab.addVar(fun.id, name, tpe, Some(value))
    }
    execSequentially(NoVal, fun.statements, (_, s) => interpretStatement(s)).map { result =>
      symTab.exitScope()
      currentExecFunctionId = None
      flowrunChannel := FlowRun.Event.EvalFunctionFinished
      result
    }

  private def interpretStatement(stmt: Statement): Future[RunVal] = Future
    .successful {
      // TODO skontat za DoWhile jer je kontra..
      // TODO skontat za for/while jer se RE-EVALUIRA EXPR!
      if !stmt.isInstanceOf[Statement.Block] then
        nextExecStatementId = Some(stmt.id)
        stepNext = false // reset the flag, wait for next statement
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
    }
    .flatMap(_ => waitForContinue())
    .flatMap { _ =>
      // println(s"interpreting: $stmt")
      import Statement.*

      stmt match {

        case d: Declare =>
          d.tpe match {
            case Type.IntegerArray =>
              Future {
                symTab.addVar(d.id, d.name, d.tpe, None)
                symTab.setValue(d.id, d.name, RunVal.IntegerArrayVal(Seq.fill(d.lengthValue)(0)))
                NoVal
              }
            case Type.RealArray =>
              Future {
                symTab.addVar(d.id, d.name, d.tpe, None)
                symTab.setValue(d.id, d.name, RunVal.RealArrayVal(Seq.fill(d.lengthValue)(0)))
                NoVal
              }
            case Type.StringArray =>
              Future {
                symTab.addVar(d.id, d.name, d.tpe, None)
                symTab.setValue(d.id, d.name, RunVal.StringArrayVal(Seq.fill(d.lengthValue)("")))
                NoVal
              }
            case Type.BooleanArray =>
              Future {
                symTab.addVar(d.id, d.name, d.tpe, None)
                symTab.setValue(d.id, d.name, RunVal.BooleanArrayVal(Seq.fill(d.lengthValue)(false)))
                NoVal
              }
            case scalar =>
              val maybeInitValueExpr = d.initValue.map(iv => parseExpr(d.id, iv))
              maybeInitValueExpr match
                case None =>
                  Future {
                    symTab.addVar(d.id, d.name, d.tpe, None)
                    NoVal
                  }
                case Some(expr) =>
                  evalExpr(d.id, expr).map { v =>
                    val promotedVal = Some(v.promote(d.id, d.name, d.tpe))
                    symTab.addVar(d.id, d.name, d.tpe, promotedVal)
                    NoVal
                  }
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
          state = State.WAITING_FOR_INPUT
          flowrunChannel := FlowRun.Event.EvalInput(id, name, prompt)
          waitForContinue().map(_ => NoVal)

        case Output(id, expr, newline) =>
          evalExpr(id, parseExpr(id, expr)).map { outputValue =>
            val newOutput = outputValue.valueString
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
          nextExecStatementId = None
          val retValFut = maybeExpr match
            case None       => Future.successful(NoVal)
            case Some(expr) => evalExpr(id, parseExpr(id, expr))
          retValFut.map { retVal =>
            val currentExecFun = programModel.ast.allFunctions.find(_.id == symTab.currentScope.id).get
            if retVal.tpe != currentExecFun.tpe then
              throw EvalException(
                s"Expected function '${currentExecFun.name}' to return '${currentExecFun.tpe}' but got '${retVal}'",
                id
              )
            else retVal
          }

        case Begin(_) =>
          Future.successful(NoVal)

        case _: Comment =>
          Future.successful(NoVal)
      }
    }
    .map { res =>
      flowrunChannel := FlowRun.Event.EvalAfterExecStatement
      res
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
                    s"Expected a Boolean but got '${otherVal}' while evaluating || operation.",
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
                    s"Expected a Boolean but got '${otherVal}' while evaluating && operation.",
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
                  s"Values '${v1}' and '${v2}' are not comparable.",
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
                    s"Cannot $op '${v1}' and '${v2}'",
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
              val nextVal = v.valueString
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
                    s"Cannot $op '${v1}' and '${v2}'",
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
              case _             => throw EvalException(s"Cant negate '${next}'", id)
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
              evalPredefinedFunction(id, f, args)
            case None =>
              val funSym = symTab.getSymbolFun(id, name)
              val fun = programModel.ast.allFunctions.find(_.name == name).get
              validateArgsNumber(id, fun.name, fun.parameters.size, args.size)
              val argsWithTypes = args.zip(fun.parameters).zipWithIndex.map { case ((arg, p), idx) =>
                if arg.tpe != p.tpe then
                  throw EvalException(
                    s"Expected: '${p}' at index $idx, got value '${arg}'",
                    id
                  )
                (p.name, p.tpe, arg)
              }
              interpretFunction(fun, argsWithTypes)
        }

  // TODO if program RUNNING TOOOOOOO LONG
  /** When program PAUSEs for input or in debug mode, we need to asynchronously wait for a continue condition to happen.
    *
    * This can be on input submission:
    *   - state is set to WAITING_FOR_INPUT in `case Input` evaluation
    *   - in setValue (called by editor) state is set to State.RUNNING so we can continue
    *
    * Adapted from https://stackoverflow.com/a/46619347/4496364
    */
  private def waitForContinue(): Future[Unit] = {
    val p = Promise[Unit]()
    // poll every PollIntervalMs to check the state of program
    // - if RUNNING set as success, continue evaluating the program
    // - if FINISHED_STOPPED set as failed
    val pingHandle = setInterval(
      PollIntervalMs, {
        if !p.isCompleted then {
          if state == State.RUNNING then {
            if execMode == ExecMode.STEP_BY_STEP then
              if stepNext then
                // stepNext = false // reset the flag, wait for next statement
                p.success({})

              // else noop, wait still
            else p.success({})
          } else if state == State.FINISHED_STOPPED then p.failure(StoppedException("Program stopped"))
        }
      }
    )

    val f = p.future
    f.onComplete { _ =>
      clearInterval(pingHandle)
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

  def evalPredefinedFunction(id: String, f: PredefinedFunction, args: Seq[RunVal]): Future[RunVal] =
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
          case n: RealVal    => Future(n.transform(Math.sin))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Cos =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.cos(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(Math.cos))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Tan =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.tan(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(Math.tan))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Ln =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.log(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(Math.log))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Log10 =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.log10(n.value.toDouble)))
          case n: RealVal    => Future(n.transform(Math.log10))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Log2 =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.log(n.value) / Math.log(2)))
          case n: RealVal    => Future(n.transform(x => Math.log(x) / Math.log(2)))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Sqrt =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: IntegerVal => Future(RealVal(Math.sqrt(n.value)))
          case n: RealVal    => Future(n.transform(Math.sqrt))
          case _             => throw EvalException(s"Expected a number argument in function ${func.name}", id)
      case func @ Pow =>
        validateArgsNumber(id, func.name, 2, args.size)
        val base = args.head
        val power = args(1)
        (base, power) match
          case (b: IntegerVal, p: IntegerVal) => Future(IntegerVal(Math.pow(b.value.toDouble, p.value.toDouble).toInt))
          case (b: IntegerVal, p: RealVal)    => Future(RealVal(Math.pow(b.value.toDouble, p.value)))
          case (b: RealVal, p: IntegerVal)    => Future(RealVal(Math.pow(b.value, p.value.toDouble)))
          case (b: RealVal, p: RealVal)       => Future(RealVal(Math.pow(b.value, p.value)))
          case _ => throw EvalException(s"Expected (Number, Number) arguments in function ${func.name}", id)

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
          case n: IntegerVal => Future(n) // noop
          case n: RealVal    => Future(IntegerVal(n.value.toInt))
          case _             => throw EvalException(s"Expected a Real argument in function ${func.name}", id)
      case func @ StringToInteger =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case n: StringVal => Future(IntegerVal(n.value.toInt))
          case _            => throw EvalException(s"Expected a String argument in function ${func.name}", id)
      // misc
      case ReadInput =>
        state = State.WAITING_FOR_INPUT
        val name = programModel.findStatement(id) match
          case stmt: Declare => stmt.name
          case stmt: Assign  => stmt.name
          case stmt: ForLoop => stmt.varName
          case _: (Return | Call | Output | If | While | DoWhile) =>
            "value"
          case other =>
            val stmtTpe = Statement.getStatType(other)
            throw EvalException(s"readInput() not supported in ${stmtTpe} statement", id)

        val prompt = args.headOption match
          case Some(s: StringVal) => Some(s.value)
          case _                  => None
        flowrunChannel := FlowRun.Event.EvalInput(id, name, prompt)
        waitForContinue().map(_ =>
          lastReadInput.getOrElse(throw EvalException(s"readInput() did not get any input", id))
        )
    }

  private def validateArgsNumber(id: String, funName: String, expected: Int, got: Int): Unit =
    if got != expected then
      throw EvalException(s"Wrong number of arguments in function $funName, expected $expected but got $got", id)
}

object Interpreter:
  val PollIntervalMs = 10
  enum State:
    case INITIALIZED
    case RUNNING
    case WAITING_FOR_INPUT
    case FINISHED_SUCCESS
    case FINISHED_STOPPED // stopped manually in editor
    case FINISHED_FAILED

  enum ExecMode:
    case NORMAL // run without stopping
    case STEP_BY_STEP // run statement by statement
    case DEBUG // run to the debug point
