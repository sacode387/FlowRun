package dev.sacode.flowrun.eval

import scala.util.*
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import reactify.*
import dev.sacode.flowrun.ast.{Expression, *}
import Expression.Type
import dev.sacode.flowrun.parse.{LexException, ParseException, Token, parseExpr}
import dev.sacode.flowrun.{FlowRun, ProgramModel, eval}
import RunVal.*
import dev.sacode.flowrun
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

import scala.util.control.NonFatal

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
  var pauseBeforeNext: Boolean = false

  var execMode: ExecMode = ExecMode.NORMAL

  var currentExecFunctionId: Option[String] = None
  var nextExecStatementId: Option[String] = None

  // input from user, needed for ReadInput function
  private var lastReadInput: Option[RunVal] = None

  def isRunning: Boolean = state == State.RUNNING || state == State.WAITING_FOR_INPUT

  def run(): Future[Unit] = run(ExecMode.NORMAL)

  def run(execMode: ExecMode): Future[Unit] = {
    this.execMode = execMode
    pauseBeforeNext = execMode == ExecMode.STEP_BY_STEP
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

  // almost same as setInputtedValue, but without any variable name
  def setLastReadInput(nodeId: String, inputValue: String): RunVal =
    val value = RunVal.fromString(inputValue)
    lastReadInput = Some(value)
    state = State.RUNNING
    value

  def setInputtedValue(nodeId: String, name: String, inputValue: String): Future[Option[RunVal]] = {
    assignInputtedValue(nodeId, name, inputValue)
      .map { v =>
        state = State.RUNNING
        pauseBeforeNext = false
        Some(v)
      }
      .recover { case (e: EvalException) => // from symbol table
        state = State.FINISHED_FAILED
        flowrunChannel := FlowRun.Event.EvalError(nodeId, e.getMessage, symTab.currentScope.id)
        None
      }
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

  private def interpretStatement(stmt: Statement): Future[RunVal] = {
    // println(s"interpreting: $stmt")
    import Statement.*

    val stmtExecFuture = stmt match {

      case Begin(_) =>
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        waitForContinue().map(_ => NoVal)

      case d: Declare =>
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        d.tpe match {
          case Type.IntegerArray =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1)).map {
                case IntegerVal(length) =>
                  if length < 0 then throw EvalException(s"Array length can not be negative: '${length}'", stmt.id)
                  symTab.addVar(d.id, d.name, d.tpe, None)
                  symTab.setValue(d.id, d.name, RunVal.IntegerArrayVal(Array.fill(length)(0)))
                  NoVal
                case other =>
                  throw EvalException(
                    s"Unsupported array length value: '${other.valueAndTypeString}'. Expected Integer",
                    stmt.id
                  )
              }
            }
          case Type.RealArray =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1)).map {
                case IntegerVal(length) =>
                  if length < 0 then throw EvalException(s"Array length can not be negative: '${length}'", stmt.id)
                  symTab.addVar(d.id, d.name, d.tpe, None)
                  symTab.setValue(d.id, d.name, RunVal.RealArrayVal(Array.fill(length)(0)))
                  NoVal
                case other =>
                  throw EvalException(
                    s"Unsupported array length value: '${other.valueAndTypeString}'. Expected Integer",
                    stmt.id
                  )
              }
            }
          case Type.StringArray =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1)).map {
                case IntegerVal(length) =>
                  if length < 0 then throw EvalException(s"Array length can not be negative: '${length}'", stmt.id)
                  symTab.addVar(d.id, d.name, d.tpe, None)
                  symTab.setValue(d.id, d.name, RunVal.StringArrayVal(Array.fill(length)("")))
                  NoVal
                case other =>
                  throw EvalException(
                    s"Unsupported array length value: '${other.valueAndTypeString}'. Expected Integer",
                    stmt.id
                  )
              }
            }
          case Type.BooleanArray =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1)).map {
                case IntegerVal(length) =>
                  if length < 0 then throw EvalException(s"Array length can not be negative: '${length}'", stmt.id)
                  symTab.addVar(d.id, d.name, d.tpe, None)
                  symTab.setValue(d.id, d.name, RunVal.BooleanArrayVal(Array.fill(length)(false)))
                  NoVal
                case other =>
                  throw EvalException(
                    s"Unsupported array length value: '${other.valueAndTypeString}'. Expected Integer",
                    stmt.id
                  )
              }
            }
          case Type.IntegerMatrix =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1))
                .zip(
                  evalExpr(stmt.id, parseExpr(stmt.id, d.lengthValue2))
                )
                .map {
                  case (IntegerVal(length1), IntegerVal(length2)) =>
                    if length1 < 0 then
                      throw EvalException(s"Matrix rows length can not be negative: '${length1}'", stmt.id)
                    if length2 < 0 then
                      throw EvalException(s"Matrix columns length can not be negative: '${length2}'", stmt.id)
                    symTab.addVar(d.id, d.name, d.tpe, None)
                    symTab.setValue(d.id, d.name, RunVal.IntegerMatrixVal(Array.fill(length1, length2)(0)))
                    NoVal
                  case (other1, other2) =>
                    throw EvalException(
                      s"Unsupported matrix length values: '${other1.valueAndTypeString}' and '${other2.valueAndTypeString}'. Expected Integers",
                      stmt.id
                    )
                }
            }
          case Type.RealMatrix =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1))
                .zip(
                  evalExpr(stmt.id, parseExpr(stmt.id, d.lengthValue2))
                )
                .map {
                  case (IntegerVal(length1), IntegerVal(length2)) =>
                    if length1 < 0 then
                      throw EvalException(s"Matrix rows length can not be negative: '${length1}'", stmt.id)
                    if length2 < 0 then
                      throw EvalException(s"Matrix columns length can not be negative: '${length2}'", stmt.id)
                    symTab.addVar(d.id, d.name, d.tpe, None)
                    symTab.setValue(d.id, d.name, RunVal.RealMatrixVal(Array.fill(length1, length2)(0)))
                    NoVal
                  case (other1, other2) =>
                    throw EvalException(
                      s"Unsupported matrix length values: '${other1.valueAndTypeString}' and '${other2.valueAndTypeString}'. Expected Integers",
                      stmt.id
                    )
                }
            }
          case Type.StringMatrix =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1))
                .zip(
                  evalExpr(stmt.id, parseExpr(stmt.id, d.lengthValue2))
                )
                .map {
                  case (IntegerVal(length1), IntegerVal(length2)) =>
                    if length1 < 0 then
                      throw EvalException(s"Matrix rows length can not be negative: '${length1}'", stmt.id)
                    if length2 < 0 then
                      throw EvalException(s"Matrix columns length can not be negative: '${length2}'", stmt.id)
                    symTab.addVar(d.id, d.name, d.tpe, None)
                    symTab.setValue(d.id, d.name, RunVal.StringMatrixVal(Array.fill(length1, length2)("")))
                    NoVal
                  case (other1, other2) =>
                    throw EvalException(
                      s"Unsupported matrix length values: '${other1.valueAndTypeString}' and '${other2.valueAndTypeString}'. Expected Integers",
                      stmt.id
                    )
                }
            }
          case Type.BooleanMatrix =>
            waitForContinue().flatMap { _ =>
              evalExpr(stmt.id, parseExpr(stmt.id, d.getLength1))
                .zip(
                  evalExpr(stmt.id, parseExpr(stmt.id, d.lengthValue2))
                )
                .map {
                  case (IntegerVal(length1), IntegerVal(length2)) =>
                    if length1 < 0 then
                      throw EvalException(s"Matrix rows length can not be negative: '${length1}'", stmt.id)
                    if length2 < 0 then
                      throw EvalException(s"Matrix columns length can not be negative: '${length2}'", stmt.id)
                    symTab.addVar(d.id, d.name, d.tpe, None)
                    symTab.setValue(d.id, d.name, RunVal.BooleanMatrixVal(Array.fill(length1, length2)(false)))
                    NoVal
                  case (other1, other2) =>
                    throw EvalException(
                      s"Unsupported matrix length values: '${other1.valueAndTypeString}' and '${other2.valueAndTypeString}'. Expected Integers",
                      stmt.id
                    )
                }
            }
          case scalar =>
            val maybeInitValueExpr = d.initValue.map(iv => parseExpr(d.id, iv))
            maybeInitValueExpr match
              case None =>
                waitForContinue().map { _ =>
                  symTab.addVar(d.id, d.name, d.tpe, None)
                  NoVal
                }
              case Some(expr) =>
                waitForContinue().flatMap { _ =>
                  evalExpr(d.id, expr).map { v =>
                    val promotedVal = Some(v.promote(d.id, d.name, d.tpe))
                    symTab.addVar(d.id, d.name, d.tpe, promotedVal)
                    NoVal
                  }
                }
        }

      case Assign(id, name, expr) =>
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        waitForContinue().flatMap { _ =>
          assignExpr(id, name, expr)
        }

      case Call(id, expr) =>
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        waitForContinue().flatMap { _ =>
          evalExpr(id, parseExpr(id, expr)).map(_ => NoVal)
        }

      case Input(id, name, prompt) =>
        val baseName = name.split("\\[").head
        if !symTab.isDeclaredVar(baseName) then throw EvalException(s"Variable '$name' is not declared.", id)
        state = State.WAITING_FOR_INPUT
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        flowrunChannel := FlowRun.Event.EvalInput(id, name, prompt)
        waitForContinue().map(_ => NoVal)

      case Output(id, expr, newline) =>
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        waitForContinue().flatMap { _ =>
          evalExpr(id, parseExpr(id, expr)).map { outputValue =>
            val newOutput = outputValue.valueString
            flowrunChannel := FlowRun.Event.EvalOutput(newOutput, newline)
            NoVal
          }
        }

      case If(id, condition, ifTrueStatements, ifFalseStatements) =>
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        waitForContinue().flatMap { _ =>
          evalExpr(id, parseExpr(id, condition)).flatMap {
            case condition: BooleanVal =>
              if (condition.value) interpretStatement(ifTrueStatements)
              else interpretStatement(ifFalseStatements)
            case condValue => throw EvalException(s"Not a valid condition: '${condValue.valueOpt.getOrElse("")}'", id)
          }
        }

      case While(id, condition, body) =>
        def loop(): Future[RunVal] = {
          pauseBeforeNext = true
          nextExecStatementId = Some(stmt.id)
          flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
          waitForContinue().flatMap { _ =>
            evalExpr(id, parseExpr(id, condition)).flatMap {
              case condition: BooleanVal =>
                if condition.value
                then
                  interpretStatement(body).flatMap { _ =>
                    loop()
                  }
                else {
                  Future.successful(NoVal)
                }
              case condValue => throw EvalException(s"Not a valid condition: '${condValue.valueOpt.getOrElse("")}'", id)
            }
          }
        }

        loop()

      case DoWhile(id, condition, body) =>
        def loop(): Future[RunVal] = {
          pauseBeforeNext = true
          nextExecStatementId = Some(stmt.id)
          flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
          waitForContinue().flatMap { _ =>
            evalExpr(id, parseExpr(id, condition)).flatMap {
              case condition: BooleanVal =>
                if condition.value then
                  interpretStatement(body).flatMap { _ =>
                    loop()
                  }
                else {
                  Future.successful(NoVal)
                }
              case condValue => throw EvalException(s"Not a valid condition: '${condValue.valueOpt.getOrElse("")}'", id)
            }
          }
        }

        interpretStatement(body).flatMap(_ => loop())

      case ForLoop(id, varName, startExpr, incrExpr, endExpr, body) =>
        def loop(conditionExpr: String, incr: Int): Future[RunVal] = {
          pauseBeforeNext = true
          nextExecStatementId = Some(stmt.id)
          flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
          waitForContinue().flatMap { _ =>
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
          }
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
        pauseBeforeNext = true
        nextExecStatementId = Some(stmt.id)
        flowrunChannel := FlowRun.Event.EvalBeforeExecStatement
        val retValFut = maybeExpr match
          case None       => waitForContinue().map(_ => NoVal)
          case Some(expr) => waitForContinue().flatMap(_ => evalExpr(id, parseExpr(id, expr)))
        retValFut.map { retVal =>
          val currentExecFun = programModel.ast.allFunctions.find(_.id == symTab.currentScope.id).get
          if retVal.tpe != currentExecFun.tpe then
            throw EvalException(
              s"Expected function '${currentExecFun.name}' to return '${currentExecFun.tpe}' but got '${retVal}'",
              id
            )
          else retVal
        }

      case _: Comment =>
        Future.successful(NoVal)
    }
    stmtExecFuture.map { res =>
      flowrunChannel := FlowRun.Event.EvalAfterExecStatement
      res
    }
  }

  private def assignExpr(id: String, name: String, expr: String): Future[RunVal] = {
    // matrix[i][j] = ..
    // array[i] = ..
    // abc = ..
    name match {
      case s"${matrixName}[${indexExpr1}][${indexExpr2}]" =>
        if !symTab.isDeclaredVar(matrixName) then throw EvalException(s"Variable '$matrixName' is not declared.", id)
        val sym = symTab.getSymbolVar(id, matrixName)
        evalExpr(id, parseExpr(id, expr)).flatMap { exprValue =>
          evalExpr(id, parseExpr(id, indexExpr1)).zip(evalExpr(id, parseExpr(id, indexExpr2))).map {
            (indexValue1, indexValue2) =>
              val index1 = indexValue1 match {
                case IntegerVal(indexValueInt) => indexValueInt
                case other =>
                  throw EvalException(
                    s"Matrix row index has to be an Integer but got: '${other.valueAndTypeString}'",
                    id
                  )
              }
              val index2 = indexValue2 match {
                case IntegerVal(indexValueInt) => indexValueInt
                case other =>
                  throw EvalException(
                    s"Matrix column index has to be an Integer but got: '${other.valueAndTypeString}'",
                    id
                  )
              }
              val matrixValue = symTab.getValue(id, matrixName)
              matrixValue match {
                case RunVal.IntegerMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  exprValue match
                    case IntegerVal(newValue) =>
                      values(index1)(index2) = newValue
                      exprValue
                    case other =>
                      throw EvalException(
                        s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'",
                        id
                      )
                case RunVal.RealMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  exprValue match
                    case RealVal(newValue) =>
                      values(index1)(index2) = newValue
                      exprValue
                    case IntegerVal(newValue) =>
                      values(index1)(index2) = newValue
                      exprValue
                    case other =>
                      throw EvalException(
                        s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'",
                        id
                      )
                case RunVal.StringMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  exprValue match
                    case StringVal(newValue) =>
                      values(index1)(index2) = newValue
                      exprValue
                    case other =>
                      throw EvalException(
                        s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'",
                        id
                      )
                case RunVal.BooleanMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  exprValue match
                    case BooleanVal(newValue) =>
                      values(index1)(index2) = newValue
                      exprValue
                    case other =>
                      throw EvalException(
                        s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'",
                        id
                      )
                case other =>
                  throw EvalException(
                    s"Cannot assign '${exprValue}' to '${matrixName}[${index1}][${index2}]' since '${matrixName}' is not a matrix",
                    id
                  )
              }
          }
        }
      case s"${arrayName}[${indexExpr}]" =>
        if !symTab.isDeclaredVar(arrayName) then throw EvalException(s"Variable '$arrayName' is not declared.", id)
        val sym = symTab.getSymbolVar(id, arrayName)
        evalExpr(id, parseExpr(id, expr)).flatMap { exprValue =>
          evalExpr(id, parseExpr(id, indexExpr)).map { indexExprValue =>
            if exprValue.valueOpt.get.toString.isEmpty && sym.tpe != Type.String then
              throw EvalException(s"Assign expression cannot be empty.", id)
            val index = indexExprValue match {
              case IntegerVal(indexValueInt) => indexValueInt
              case other =>
                throw EvalException(s"Array index has to be an Integer but got: '${other.valueAndTypeString}'", id)
            }
            val arrayValue = symTab.getValue(id, arrayName)
            arrayValue match {
              case RunVal.IntegerArrayVal(values) =>
                if !values.indices.contains(index) then
                  throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
                exprValue match
                  case IntegerVal(newValue) =>
                    values(index) = newValue
                    exprValue
                  case other =>
                    throw EvalException(s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'", id)
              case RunVal.RealArrayVal(values) =>
                if !values.indices.contains(index) then
                  throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
                exprValue match
                  case RealVal(newValue) =>
                    values(index) = newValue
                    exprValue
                  case IntegerVal(newValue) =>
                    values(index) = newValue
                    exprValue
                  case other =>
                    throw EvalException(s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'", id)
              case RunVal.StringArrayVal(values) =>
                if !values.indices.contains(index) then
                  throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
                exprValue match
                  case StringVal(newValue) =>
                    values(index) = newValue
                    exprValue
                  case other =>
                    throw EvalException(s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'", id)
              case RunVal.BooleanArrayVal(values) =>
                if !values.indices.contains(index) then
                  throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
                exprValue match
                  case BooleanVal(newValue) =>
                    values(index) = newValue
                    exprValue
                  case other =>
                    throw EvalException(s"Cannot assign '${other.valueAndTypeString}' to an item of '${sym.asVar}'", id)
              case other =>
                throw EvalException(
                  s"Cannot assign '${exprValue}' to '${arrayName}[${index}]' since '${arrayName}' is not an array",
                  id
                )
            }
          }
        }
      case _ =>
        if !symTab.isDeclaredVar(name) then throw EvalException(s"Variable '$name' is not declared.", id)
        val sym = symTab.getSymbolVar(id, name)
        evalExpr(id, parseExpr(id, expr)).map { exprValue =>
          if exprValue.valueOpt.get.toString.isEmpty && sym.tpe != Type.String then
            throw EvalException(s"Assign expression cannot be empty.", id)
          val promotedVal = exprValue.promote(id, name, sym.tpe)
          symTab.setValue(id, name, promotedVal)
          exprValue
        }
    }
  }

  private def assignInputtedValue(id: String, name: String, inputValue: String): Future[RunVal] = {
    // matrix[i][j] = ..
    // array[i] = ..
    // abc = ..
    name match {
      case s"${matrixName}[${index1Expr}][${index2Expr}]" =>
        if !symTab.isDeclaredVar(matrixName) then throw EvalException(s"Variable '$matrixName' is not declared.", id)
        val sym = symTab.getSymbolVar(id, matrixName)
        val arrayValue = symTab.getValue(id, matrixName)
        evalExpr(id, parseExpr(id, index1Expr))
          .zip(evalExpr(id, parseExpr(id, index2Expr)))
          .map {
            case (IntegerVal(index1), IntegerVal(index2)) =>
              arrayValue match
                case RunVal.IntegerMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  try {
                    val value = inputValue.toInt
                    values(index1)(index2) = value
                    IntegerVal(value)
                  } catch {
                    case _: NumberFormatException =>
                      throw EvalException(
                        s"You entered invalid value for '${matrixName}[${index1}][${index2}]' : '${inputValue}'.",
                        id
                      )
                  }
                case RunVal.RealMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  try {
                    val value = inputValue.toDouble
                    values(index1)(index2) = value
                    RealVal(value)
                  } catch {
                    case _: NumberFormatException =>
                      throw EvalException(
                        s"You entered invalid value for '${matrixName}[${index1}][${index2}]' : '${inputValue}'.",
                        id
                      )
                  }
                case RunVal.StringMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  values(index1)(index2) = inputValue
                  StringVal(inputValue)
                case RunVal.BooleanMatrixVal(values) =>
                  if !values.indices.contains(index1) then
                    throw EvalException(s"Row index out of bounds: '${index1}' (0..${values.length - 1})", id)
                  if !values(index1).indices.contains(index2) then
                    throw EvalException(
                      s"Column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                      id
                    )
                  try {
                    val value = inputValue.toBoolean
                    values(index1)(index2) = value
                    BooleanVal(value)
                  } catch {
                    case _: IllegalArgumentException =>
                      throw EvalException(
                        s"You entered invalid value for '${matrixName}[${index1}][${index2}]' : '${inputValue}'.",
                        id
                      )
                  }
                case _ =>
                  throw EvalException(
                    s"Cannot assign '${inputValue}' to element in ${matrixName} since it is not a matrix",
                    id
                  )
            case other => throw EvalException(s"Invalid array index type: '$other'. Expected an Integer", id)
          }
      case s"${arrayName}[${indexExpr}]" =>
        if !symTab.isDeclaredVar(arrayName) then throw EvalException(s"Variable '$arrayName' is not declared.", id)
        val sym = symTab.getSymbolVar(id, arrayName)
        val arrayValue = symTab.getValue(id, arrayName)
        evalExpr(id, parseExpr(id, indexExpr)).map { indexExprValue =>
          val index = indexExprValue match {
            case IntegerVal(value) => value
            case other => throw EvalException(s"Invalid array index type: '$other'. Expected an Integer", id)
          }
          arrayValue match
            case RunVal.IntegerArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Index out of bounds: '${index}' (0..${values.length - 1})", id)
              try {
                val value = inputValue.toInt
                values(index) = value
                IntegerVal(value)
              } catch {
                case _: NumberFormatException =>
                  throw EvalException(s"You entered invalid value for '${arrayName}[${index}]' : '${inputValue}'.", id)
              }
            case RunVal.RealArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Index out of bounds: '${index}' (0..${values.length - 1})", id)
              try {
                val value = inputValue.toDouble
                values(index) = value
                RealVal(value)
              } catch {
                case _: NumberFormatException =>
                  throw EvalException(s"You entered invalid value for '${arrayName}[${index}]' : '${inputValue}'.", id)
              }
            case RunVal.StringArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Index out of bounds: '${index}' (0..${values.length - 1})", id)
              values(index) = inputValue
              StringVal(inputValue)
            case RunVal.BooleanArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Index out of bounds: '${index}' (0..${values.length - 1})", id)
              try {
                val value = inputValue.toBoolean
                values(index) = value
                BooleanVal(value)
              } catch {
                case _: IllegalArgumentException =>
                  throw EvalException(s"You entered invalid value for '${arrayName}[${index}]' : '${inputValue}'.", id)
              }
            case _ =>
              throw EvalException(
                s"Cannot assign '${inputValue}' to '${arrayName}[${index}]' since '${arrayName}' is not an array",
                id
              )
        }
      case _ =>
        if !symTab.isDeclaredVar(name) then throw EvalException(s"Variable '$name' is not declared.", id)
        val sym = symTab.getSymbolVar(id, name)
        Future {
          sym.tpe match
            case Type.Integer =>
              try {
                val runVal = IntegerVal(inputValue.toInt)
                symTab.setValue(id, name, runVal)
                runVal
              } catch {
                case _: NumberFormatException =>
                  throw EvalException(s"You entered invalid value for '${sym.asVar}' : '${inputValue}'.", id)
              }
            case Type.Real =>
              try {
                val runVal = RealVal(inputValue.toDouble)
                symTab.setValue(id, name, runVal)
                runVal
              } catch {
                case _: NumberFormatException =>
                  throw EvalException(s"You entered invalid value for '${sym.asVar}' : '${inputValue}'.", id)
              }
            case Type.Boolean =>
              try {
                val runVal = BooleanVal(inputValue.toBoolean)
                symTab.setValue(id, name, runVal)
                runVal
              } catch {
                case _: IllegalArgumentException =>
                  throw EvalException(s"You entered invalid value for '${sym.asVar}' : '${inputValue}'.", id)
              }
            case Type.String =>
              val runVal = StringVal(inputValue)
              symTab.setValue(id, name, runVal)
              runVal
            case other => throw EvalException(s"Input of type '${other.pretty}' is not supported.", id)
        }
    }
  }

  // must to be a Future bcoz readInput() ...
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
              case (v1: IntegerArrayVal, v2: IntegerArrayVal) =>
                if isEquals then BooleanVal(v1.values.toSeq == v2.values.toSeq)
                else BooleanVal(v1.values.toSeq != v2.values.toSeq)
              case (v1: RealArrayVal, v2: RealArrayVal) =>
                if isEquals then BooleanVal(v1.values.toSeq == v2.values.toSeq)
                else BooleanVal(v1.values.toSeq != v2.values.toSeq)
              case (v1: StringArrayVal, v2: StringArrayVal) =>
                if isEquals then BooleanVal(v1.values.toSeq == v2.values.toSeq)
                else BooleanVal(v1.values.toSeq != v2.values.toSeq)
              case (v1: BooleanArrayVal, v2: BooleanArrayVal) =>
                if isEquals then BooleanVal(v1.values.toSeq == v2.values.toSeq)
                else BooleanVal(v1.values.toSeq != v2.values.toSeq)
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
                  else if isDiv then {
                    try IntegerVal(v1.value / v2.value)
                    catch
                      case _: ArithmeticException =>
                        throw EvalException(s"Division by zero ${v1.value} / ${v2.value}", id)
                  } else {
                    try IntegerVal(v1.value % v2.value)
                    catch
                      case _: ArithmeticException =>
                        throw EvalException(s"Division by zero ${v1.value} / ${v2.value}", id)
                  }
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
      case ArrayIndexAccess(arrayName, indexExpr) =>
        evalExpr(id, indexExpr).map { indexValue =>
          val arr = symTab.getValue(id, arrayName)
          val index = indexValue match {
            case IntegerVal(indexValueInt) => indexValueInt
            case other =>
              throw EvalException(s"Array index has to be an Integer but got: '${other.valueAndTypeString}'", id)
          }
          arr match {
            case IntegerArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
              IntegerVal(values(index))
            case RealArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
              RealVal(values(index))
            case StringArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
              StringVal(values(index))
            case BooleanArrayVal(values) =>
              if !values.indices.contains(index) then
                throw EvalException(s"Array index out of bounds: '${index}' (0..${values.length - 1})", id)
              BooleanVal(values(index))
            case other =>
              throw EvalException(s"Cannot index into '${other}' because it is not an array", id)
          }
        }
      case MatrixIndexAccess(matrixName, indexExpr1, indexExpr2) =>
        evalExpr(id, indexExpr1).zip(evalExpr(id, indexExpr2)).map { (indexValue1, indexValue2) =>
          val arr = symTab.getValue(id, matrixName)
          val index1 = indexValue1 match {
            case IntegerVal(indexValueInt) => indexValueInt
            case other =>
              throw EvalException(s"Matrix row index has to be an Integer but got: '${other.valueAndTypeString}'", id)
          }
          val index2 = indexValue2 match {
            case IntegerVal(indexValueInt) => indexValueInt
            case other =>
              throw EvalException(
                s"Matrix column index has to be an Integer but got: '${other.valueAndTypeString}'",
                id
              )
          }
          arr match {
            case IntegerMatrixVal(values) =>
              if !values.indices.contains(index1) then
                throw EvalException(s"Matrix row index out of bounds: '${index1}' (0..${values.length - 1})", id)
              if !values(index1).indices.contains(index2) then
                throw EvalException(
                  s"Matrix column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                  id
                )
              IntegerVal(values(index1)(index2))
            case RealMatrixVal(values) =>
              if !values.indices.contains(index1) then
                throw EvalException(s"Matrix row index out of bounds: '${index1}' (0..${values.length - 1})", id)
              if !values(index1).indices.contains(index2) then
                throw EvalException(
                  s"Matrix column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                  id
                )
              RealVal(values(index1)(index2))
            case StringMatrixVal(values) =>
              if !values.indices.contains(index1) then
                throw EvalException(s"Matrix row index out of bounds: '${index1}' (0..${values.length - 1})", id)
              if !values(index1).indices.contains(index2) then
                throw EvalException(
                  s"Matrix column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                  id
                )
              StringVal(values(index1)(index2))
            case BooleanMatrixVal(values) =>
              if !values.indices.contains(index1) then
                throw EvalException(s"Matrix row index out of bounds: '${index1}' (0..${values.length - 1})", id)
              if !values(index1).indices.contains(index2) then
                throw EvalException(
                  s"Matrix column index out of bounds: '${index2}' (0..${values(index1).length - 1})",
                  id
                )
              BooleanVal(values(index1)(index2))
            case other =>
              throw EvalException(s"Cannot index into '${other}' because it is not an array", id)
          }
        }
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
              val _ = symTab.getSymbolFun(id, name) // make sure it is defined
              val fun = programModel.ast.allFunctions.find(_.name == name).get
              validateArgsNumber(id, fun.name, fun.parameters.size, args.size)
              val argsWithTypes = args.zip(fun.parameters).zipWithIndex.map { case ((arg, p), idx) =>
                if arg.tpe != p.tpe then
                  throw EvalException(
                    s"Expected: '${p.pretty}' at index $idx, got value '${arg}'",
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
            if execMode == ExecMode.STEP_BY_STEP then {
              // proceed with execution
              if !pauseBeforeNext then p.success({})
            } else p.success({})
          } else if state == State.FINISHED_STOPPED then {
            p.failure(StoppedException("Program stopped"))
          }
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

      // strings and arrays
      case func @ Length =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case s: StringVal       => Future(IntegerVal(s.value.length))
          case a: IntegerArrayVal => Future(IntegerVal(a.values.length))
          case a: RealArrayVal    => Future(IntegerVal(a.values.length))
          case a: StringArrayVal  => Future(IntegerVal(a.values.length))
          case a: BooleanArrayVal => Future(IntegerVal(a.values.length))
          case _ => throw EvalException(s"Expected String or Array argument in function ${func.name}", id)
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
      // matrices
      case func @ NumRows =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case m: IntegerMatrixVal => Future(IntegerVal(m.values.length))
          case m: RealMatrixVal    => Future(IntegerVal(m.values.length))
          case m: StringMatrixVal  => Future(IntegerVal(m.values.length))
          case m: BooleanMatrixVal => Future(IntegerVal(m.values.length))
          case _                   => throw EvalException(s"Expected Matrix argument in function ${func.name}", id)
      case func @ NumCols =>
        validateArgsNumber(id, func.name, 1, args.size)
        args.head match
          case m: IntegerMatrixVal => Future(IntegerVal(if m.values.isEmpty then 0 else m.values(0).length))
          case m: RealMatrixVal    => Future(IntegerVal(if m.values.isEmpty then 0 else m.values(0).length))
          case m: StringMatrixVal  => Future(IntegerVal(if m.values.isEmpty then 0 else m.values(0).length))
          case m: BooleanMatrixVal => Future(IntegerVal(if m.values.isEmpty then 0 else m.values(0).length))
          case _                   => throw EvalException(s"Expected Matrix argument in function ${func.name}", id)
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
      case ClearOutput =>
        flowrunChannel := FlowRun.Event.ClearOutput()
        Future.successful(NoVal)
    }

  private def validateArgsNumber(id: String, funName: String, expected: Int, got: Int): Unit =
    if got != expected then
      throw EvalException(s"Wrong number of arguments in function $funName, expected $expected but got $got", id)
}

object Interpreter:
  private val PollIntervalMs = 10
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
