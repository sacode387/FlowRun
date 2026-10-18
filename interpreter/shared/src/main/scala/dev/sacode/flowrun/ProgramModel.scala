package dev.sacode.flowrun

import reactify.*
import dev.sacode.flowrun.ast.*, Statement.*

class ProgramModel(
    initAst: Program,
    flowrunChannel: Channel[FlowRun.Event]
) {
  import ProgramModel.*

  private var currentAst: Program = initAst
  def ast: Program = currentAst

  private case class Snapshot(program: Program, functionId: String, statementId: Option[String])
  private var past = List.empty[Snapshot]
  private var future = List.empty[Snapshot]
  private var textEditPending = false
  private var textGroupRecorded = false

  def canUndo: Boolean = past.nonEmpty
  def canRedo: Boolean = future.nonEmpty

  // Called before a text input event; only its mutation may join the current typing group.
  def recordTextEdit(): Unit = textEditPending = true

  def endEditGroup(): Unit =
    textEditPending = false
    textGroupRecorded = false

  private def snapshot: Snapshot = Snapshot(ast, currentSelectedFunctionId, currentSelectedStmtId)

  private def commit(next: Program, event: FlowRun.Event)(select: => Unit = ()): Unit =
    val typing = textEditPending
    textEditPending = false
    if next != ast then
      if !typing || !textGroupRecorded then past = (snapshot :: past).take(100)
      future = Nil
      textGroupRecorded = typing
      currentAst = next.copy(revision = math.max(ast.revision, next.revision) + 1)
      select
      flowrunChannel := event

  def replaceProgram(program: Program): Unit =
    endEditGroup()
    commit(program, FlowRun.Event.ProgramReplaced) {
      currentSelectedFunctionId = MainFunId
      currentSelectedStmtId = None
    }

  def undo(): Unit =
    past.headOption.foreach { previous =>
      future = snapshot :: future
      past = past.tail
      restore(previous)
    }

  def redo(): Unit =
    future.headOption.foreach { next =>
      past = (snapshot :: past).take(100)
      future = future.tail
      restore(next)
    }

  private def restore(saved: Snapshot): Unit =
    endEditGroup()
    currentAst = saved.program.copy(revision = ast.revision + 1)
    currentSelectedFunctionId = saved.functionId
    if !ast.allFunctions.exists(_.id == currentSelectedFunctionId) then currentSelectedFunctionId = MainFunId
    currentSelectedStmtId = saved.statementId.filter { id =>
      FunctionModel(currentFunction).findById(currentFunction.statements, id).nonEmpty
    }
    flowrunChannel := FlowRun.Event.ProgramReplaced

  // global vars go brrrrr
  var currentSelectedFunctionId: String = MainFunId
  var currentSelectedStmtId: Option[String] = None

  def setName(name: String): Unit =
    commit(ast.copy(name = name), FlowRun.Event.FunctionUpdated)()

  def setConfig(config: FlowRunConfig): Unit =
    commit(ast.copy(config = config), FlowRun.Event.ConfigChanged)()

  def currentFunction: Function =
    if currentSelectedFunctionId == MainFunId then ast.main
    else ast.functions.find(_.id == currentSelectedFunctionId).get

  def addFunction(fun: Function): Unit =
    val newFunctions = ast.functions.appended(fun)
    commit(ast.copy(functions = newFunctions), FlowRun.Event.FunctionUpdated) {
      currentSelectedFunctionId = fun.id
      currentSelectedStmtId = None
    }
    flowrunChannel := FlowRun.Event.Deselected

  // Calls in the body retain their original targets, including recursive calls.
  def duplicateFunction(id: String): Unit =
    val source = ast.functions.find(_.id == id).getOrElse {
      throw IllegalArgumentException(s"Cannot duplicate function $id")
    }
    val names = ast.allFunctions.map(_.name).toSet
    val base = s"${source.name}Copy"
    val name = Iterator.from(1).map(n => if n == 1 then base else s"$base$n").find(!names(_)).get
    addFunction(
      source.copy(
        rawId = AST.newId,
        name = name,
        parameters = source.parameters.map(_.copy(id = AST.newId)),
        statements = source.statements.map(_.duplicated)
      )
    )

  def addFunction(): Unit =
    val lastFunNum = ast.functions
      .map(_.name.drop(3))
      .flatMap(_.toIntOption)
      .maxOption
      .getOrElse(0)
    val newFunName = "fun" + (lastFunNum + 1)
    val newFun = Function(
      AST.newId,
      newFunName,
      statements = List(Begin(AST.newId), Return(AST.newId))
    )
    addFunction(newFun)

  def deleteFunction(id: String): Unit =
    val newFunctions = ast.functions.filterNot(_.id == id)
    commit(ast.copy(functions = newFunctions), FlowRun.Event.FunctionUpdated) {
      currentSelectedFunctionId = MainFunId
      currentSelectedStmtId = None
    }
    flowrunChannel := FlowRun.Event.Deselected

  def updateFunction(
      id: String,
      name: Option[String] = None,
      tpe: Option[Expression.Type] = None,
      parameters: Option[List[Function.Parameter]] = None
  ) =
    val newFunctions = ast.functions.map { f =>
      if f.id == currentFunction.id then
        var updatedFun = f
        name.foreach(n => updatedFun = updatedFun.copy(name = n))
        tpe.foreach(t => updatedFun = updatedFun.copy(tpe = t))
        parameters.foreach { params =>
          updatedFun = updatedFun.copy(parameters = params)
        }
        updatedFun
      else f
    }
    commit(ast.copy(functions = newFunctions), FlowRun.Event.FunctionUpdated)()

  /* per-function */
  def addStmt(stmt: Statement, blockId: String, afterId: String): Unit =
    update(_.addStmt(stmt, blockId, afterId), FlowRun.Event.StmtAdded)

  def updateStmt(stmt: Statement): Unit =
    update(_.updateStmt(stmt), FlowRun.Event.StmtUpdated(stmt.id))

  def delete(id: String): Unit =
    update(_.delete(id), FlowRun.Event.StmtDeleted)

  def findStatement(stmtId: String): Statement =
    FunctionModel(currentFunction).doFind(stmtId)

  private def update(transform: FunctionModel => FunctionModel, evt: FlowRun.Event): Unit = {
    val newFunction = transform(FunctionModel(currentFunction)).ast
    val next =
      if currentFunction.isMain then ast.copy(main = newFunction)
      else
        val idx = ast.functions.indexWhere(_.id == currentSelectedFunctionId)
        ast.copy(functions = ast.functions.updated(idx, newFunction))
    commit(next, evt)()
  }
}

object ProgramModel:
  val MainFunId = "fun-main"
end ProgramModel

case class FunctionModel(
    ast: Function
) {

  def addStmt(stmt: Statement, blockId: String, afterId: String): FunctionModel =
    doInsert(stmt, blockId, afterId)

  def updateStmt(stmt: Statement): FunctionModel =
    doUpdate(stmt.id, stmt)

  def delete(id: String): FunctionModel =
    val newStats = delete(ast.statements, id)
    this.copy(ast = ast.copy(statements = newStats))

  /* HELPERS */
  private def doInsert(newStatement: Statement, blockId: String, afterId: String): FunctionModel =
    val newStats = insert(ast.statements, ast.id, newStatement, blockId, afterId)
    this.copy(ast = ast.copy(statements = newStats))

  private def insert(
      statements: List[Statement],
      currentBlockId: String,
      newStatement: Statement,
      blockId: String,
      afterId: String
  ): List[Statement] = {
    // println((currentBlockId, newStatement, blockId, afterId))

    if currentBlockId == blockId then
      val afterStatementIdx = statements.indexWhere(_.id == afterId)
      statements.patch(afterStatementIdx + 1, List(newStatement), 0)
    else
      statements.map {
        case stmt: If =>
          val updatedTrueBlock = stmt.trueBlock.copy(
            statements = insert(stmt.trueBlock.statements, stmt.trueBlock.id, newStatement, blockId, afterId)
          )
          val updatedFalseBlock = stmt.falseBlock.copy(
            statements = insert(stmt.falseBlock.statements, stmt.falseBlock.id, newStatement, blockId, afterId)
          )
          stmt.copy(trueBlock = updatedTrueBlock, falseBlock = updatedFalseBlock)

        case stmt: While =>
          val updatedBlock = stmt.body.copy(
            statements = insert(stmt.body.statements, stmt.body.id, newStatement, blockId, afterId)
          )
          stmt.copy(body = updatedBlock)

        case stmt: DoWhile =>
          val updatedBlock = stmt.body.copy(
            statements = insert(stmt.body.statements, stmt.body.id, newStatement, blockId, afterId)
          )
          stmt.copy(body = updatedBlock)

        case stmt: ForLoop =>
          val updatedBlock = stmt.body.copy(
            statements = insert(stmt.body.statements, stmt.body.id, newStatement, blockId, afterId)
          )
          stmt.copy(body = updatedBlock)

        case other => other
      }
  }

  private def doUpdate(statementId: String, newStatement: Statement): FunctionModel =
    val newStats = update(ast.statements, statementId, newStatement)
    // println(s"OLD: ${ast.statements} \nNEW: $newStatement, \nNEW: $newStats")
    this.copy(ast = ast.copy(statements = newStats))

  private def update(
      statements: List[Statement],
      statementId: String,
      newStatement: Statement
  ): List[Statement] = {
    val statementIdx = statements.indexWhere(_.id == statementId)
    if (statementIdx >= 0) {
      val existingStatement = statements(statementIdx)
      if (existingStatement.getClass != newStatement.getClass) {
        throw RuntimeException(
          s"Statement type mismatch. Existing: ${existingStatement.getClass} New: ${newStatement.getClass}"
        )
      }
      statements.updated(statementIdx, newStatement)
    } else {
      statements.map {
        case ifStatement: If =>
          ifStatement.copy(
            trueBlock = ifStatement.trueBlock
              .copy(statements = update(ifStatement.trueBlock.statements, statementId, newStatement)),
            falseBlock = ifStatement.falseBlock.copy(
              statements = update(ifStatement.falseBlock.statements, statementId, newStatement)
            )
          )
        case whileStatement: While =>
          whileStatement.copy(
            body =
              whileStatement.body.copy(statements = update(whileStatement.body.statements, statementId, newStatement))
          )
        case doWhileStatement: DoWhile =>
          doWhileStatement.copy(
            body = doWhileStatement.body.copy(statements =
              update(doWhileStatement.body.statements, statementId, newStatement)
            )
          )
        case stmt: ForLoop =>
          stmt.copy(
            body = stmt.body.copy(statements = update(stmt.body.statements, statementId, newStatement))
          )
        case simple =>
          simple
      }
    }
  }

  private def delete(
      statements: List[Statement],
      statementId: String
  ): List[Statement] = {
    statements.flatMap {
      case Block(_, blockStats) =>
        delete(blockStats, statementId)
      case stat @ If(id, expr, trueBlock, falseBlock) =>
        if statementId == id then List.empty
        else
          val newStat = stat
            .copy(trueBlock = trueBlock.copy(statements = delete(trueBlock.statements, statementId)))
            .copy(falseBlock = falseBlock.copy(statements = delete(falseBlock.statements, statementId)))
          List(newStat)
      case stat @ While(id, expr, body) =>
        if statementId == id then List.empty
        else
          val newStat = stat
            .copy(body = body.copy(statements = delete(body.statements, statementId)))
          List(newStat)
      case stat @ DoWhile(id, expr, body) =>
        if statementId == id then List.empty
        else
          val newStat = stat
            .copy(body = body.copy(statements = delete(body.statements, statementId)))
          List(newStat)
      case stat: ForLoop =>
        if statementId == stat.id then List.empty
        else
          val newStat = stat
            .copy(body = stat.body.copy(statements = delete(stat.body.statements, statementId)))
          List(newStat)
      case st =>
        Option.unless(st.id == statementId)(st)
    }
  }

  def doFind(statementId: String): Statement =
    findById(ast.statements, statementId).get

  private[flowrun] def findById(
      statements: List[Statement],
      statementId: String
  ): Option[Statement] =
    statements.flatMap {
      case Block(_, blockStats) =>
        findById(blockStats, statementId)
      case ifStat @ If(id, expr, trueBlock, falseBlock) =>
        Option
          .when(statementId == id)(ifStat)
          .orElse(findById(trueBlock.statements, statementId))
          .orElse(findById(falseBlock.statements, statementId))
      case whileStat @ While(id, expr, body) =>
        Option
          .when(statementId == id)(whileStat)
          .orElse(findById(body.statements, statementId))
      case doWhileStat @ DoWhile(id, expr, body) =>
        Option
          .when(statementId == id)(doWhileStat)
          .orElse(findById(body.statements, statementId))
      case whileStat @ ForLoop(id, _, _, _, _, body) =>
        Option
          .when(statementId == id)(whileStat)
          .orElse(findById(body.statements, statementId))
      case stmt =>
        Option.when(stmt.id == statementId)(stmt)
    }.headOption

}
