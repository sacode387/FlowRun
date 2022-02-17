package dev.sacode.flowrun

import scala.scalajs.js
import reactify.*
import dev.sacode.flowrun.parse.parseExpr

class ProgramModel(
    initAst: Program,
    flowrunChannel: Channel[FlowRun.Event]
) {
  import ProgramModel.*, Request.*

  var ast = initAst

  // global vars go brrrrr
  var currentFunctionId: String = MainFunId
  // currently selected node
  var currentStmtId: Option[String] = None
  // currently selected/rclicked edge
  var currentEdgeId: Option[String] = None

  def currentFunction: Function =
    if currentFunctionId == MainFunId then ast.main
    else ast.functions.find(_.id == currentFunctionId).get

  def addFunction(fun: Function): Unit =
    val newFunctions = ast.functions.appended(fun)
    ast = ast.copy(functions = newFunctions)
    currentFunctionId = fun.id
    flowrunChannel := FlowRun.Event.Deselected
    flowrunChannel := FlowRun.Event.FunctionUpdated

  def addNewFunction(): Unit =
    val lastFunNum = ast.functions
      .map(_.name.substring(3))
      .flatMap(_.toIntOption)
      .maxOption
      .getOrElse(0)
    val newFunName = "fun" + (lastFunNum + 1)
    val newFun = Function(
      AST.newId,
      newFunName,
      statements = List(Statement.Begin(AST.newId), Statement.Return(AST.newId))
    )
    addFunction(newFun)

  def deleteFunction(id: String): Unit =
    val newFunctions = ast.functions.filterNot(_.id == id)
    ast = ast.copy(functions = newFunctions)
    currentFunctionId = MainFunId
    flowrunChannel := FlowRun.Event.Deselected
    flowrunChannel := FlowRun.Event.FunctionUpdated

  def updateFunction(req: UpdateFunction) =
    val newFunctions = ast.functions.map { f =>
      if f.id == currentFunction.id then
        var updatedFun = f
        req.name.foreach(n => updatedFun = updatedFun.copy(name = n))
        req.tpe.foreach(t => updatedFun = updatedFun.copy(tpe = t))
        req.parameters.foreach { params =>
          updatedFun = updatedFun.copy(parameters = params)
        }
        updatedFun
      else f
    }
    ast = ast.copy(functions = newFunctions)
    flowrunChannel := FlowRun.Event.FunctionUpdated

  /* per-function */
  def addStmt(req: AddStmt): Unit =
    update(_.addStmt(req), FlowRun.Event.StmtAdded)

  def addDeclare(req: AddDeclare): Unit =
    update(_.addDeclare(req), FlowRun.Event.StmtAdded)

  def addAssign(req: AddAssign): Unit =
    update(_.addAssign(req), FlowRun.Event.StmtAdded)

  def addOutput(req: AddOutput): Unit =
    update(_.addOutput(req), FlowRun.Event.StmtAdded)

  def addInput(req: AddInput): Unit =
    update(_.addInput(req), FlowRun.Event.StmtAdded)

  def addCall(req: AddCall): Unit =
    update(_.addCall(req), FlowRun.Event.StmtAdded)

  def addIf(req: AddIf): Unit =
    update(_.addIf(req), FlowRun.Event.StmtAdded)

  def addWhile(req: AddWhile): Unit =
    update(_.addWhile(req), FlowRun.Event.StmtAdded)

  def addDoWhile(req: AddDoWhile): Unit =
    update(_.addDoWhile(req), FlowRun.Event.StmtAdded)

  def addForLoop(req: AddForLoop): Unit =
    update(_.addForLoop(req), FlowRun.Event.StmtAdded)

  def updateDeclare(req: UpdateDeclare): Unit =
    update(_.updateDeclare(req), FlowRun.Event.SyntaxSuccess)

  def updateAssign(req: UpdateAssign): Unit =
    update(_.updateAssign(req), FlowRun.Event.SyntaxSuccess)

  def updateOutput(req: UpdateOutput): Unit =
    update(_.updateOutput(req), FlowRun.Event.SyntaxSuccess)

  def updateInput(req: UpdateInput): Unit =
    update(_.updateInput(req), FlowRun.Event.SyntaxSuccess)

  def updateCall(req: UpdateCall): Unit =
    update(_.updateCall(req), FlowRun.Event.SyntaxSuccess)

  def updateReturn(req: UpdateReturn): Unit =
    update(_.updateReturn(req), FlowRun.Event.SyntaxSuccess)

  def updateIf(req: UpdateIf): Unit =
    update(_.updateIf(req), FlowRun.Event.SyntaxSuccess)

  def updateWhile(req: UpdateWhile): Unit =
    update(_.updateWhile(req), FlowRun.Event.SyntaxSuccess)

  def updateDoWhile(req: UpdateDoWhile): Unit =
    update(_.updateDoWhile(req), FlowRun.Event.SyntaxSuccess)

  def updateForLoop(req: UpdateForLoop): Unit =
    update(_.updateForLoop(req), FlowRun.Event.SyntaxSuccess)

  def delete(req: Delete): Unit =
    update(_.delete(req), FlowRun.Event.StmtDeleted)

  def findStatement(stmtId: String): Statement =
    FunctionModel(currentFunction).doFind(stmtId)

  private def update(transform: FunctionModel => FunctionModel, evt: FlowRun.Event): Unit = {
    val newFunction = transform(FunctionModel(currentFunction)).ast
    if currentFunction.isMain then ast = ast.copy(main = newFunction)
    else
      ast.functions.indexWhere(_.id == currentFunctionId) match
        case -1 =>
          println(s"Oops, function $currentFunctionId does not exist...")
        case idx =>
          val newFunctions = ast.functions.updated(idx, newFunction)
          ast = ast.copy(functions = newFunctions)

    flowrunChannel := evt
  }
}

object ProgramModel:
  import dev.sacode.flowrun.Expression.Type

  val MainFunId = "fun-main"

  // TODO refactor to AddStmt, remove redundancy
  enum Request:
    case Delete(id: String)
    case AddStmt(stmt: Statement, afterId: String, blockId: String)
    case AddDeclare(id: String, name: String, tpe: Type, afterId: String, blockId: String)
    case AddAssign(id: String, afterId: String, blockId: String)
    case AddOutput(id: String, afterId: String, blockId: String)
    case AddInput(id: String, afterId: String, blockId: String)
    case AddCall(id: String, afterId: String, blockId: String)
    case AddIf(
        id: String,
        trueId: String,
        falseId: String,
        afterId: String,
        blockId: String
    )
    case AddWhile(
        id: String,
        bodyId: String,
        afterId: String,
        blockId: String
    )
    case AddDoWhile(
        id: String,
        bodyId: String,
        afterId: String,
        blockId: String
    )
    case AddForLoop(
        id: String,
        bodyId: String,
        afterId: String,
        blockId: String
    )

    case UpdateDeclare(
        id: String,
        name: Option[String] = None,
        tpe: Option[Type] = None,
        expr: Option[Option[String]] = None
    )
    case UpdateAssign(id: String, name: Option[String] = None, expr: Option[String] = None)
    case UpdateOutput(id: String, expr: String)
    case UpdateInput(id: String, name: String)
    case UpdateCall(id: String, expr: String)
    case UpdateReturn(id: String, expr: Option[Option[String]] = None)
    case UpdateIf(id: String, expr: String)
    case UpdateWhile(id: String, expr: String)
    case UpdateDoWhile(id: String, expr: String)
    case UpdateForLoop(
        id: String,
        varName: Option[String] = None,
        start: Option[String] = None,
        incr: Option[String] = None,
        end: Option[String] = None
    )
    case UpdateFunction(
        id: String,
        name: Option[String] = None,
        tpe: Option[Type] = None,
        parameters: Option[List[Function.Parameter]] = None
    )
end ProgramModel

case class FunctionModel(
    ast: Function
) {
  import ProgramModel.Request.*

  def addStmt(req: AddStmt): FunctionModel =
    doInsert(req.afterId, req.stmt, req.blockId)

  def addDeclare(req: AddDeclare): FunctionModel =
    val newStat = Statement.Declare(req.id, req.name, req.tpe, None)
    doInsert(req.afterId, newStat, req.blockId)

  def addAssign(req: AddAssign): FunctionModel =
    val newStat = Statement.Assign(req.id, "x", "19")
    doInsert(req.afterId, newStat, req.blockId)

  def addOutput(req: AddOutput): FunctionModel =
    val newStat = Statement.Output(req.id, "\"output\"")
    doInsert(req.afterId, newStat, req.blockId)

  def addInput(req: AddInput): FunctionModel =
    val newStat = Statement.Input(req.id, "x")
    doInsert(req.afterId, newStat, req.blockId)

  def addCall(req: AddCall): FunctionModel =
    val newStat = Statement.Call(req.id, "fun1()")
    doInsert(req.afterId, newStat, req.blockId)

  def addIf(req: AddIf): FunctionModel =
    val newStat = Statement.If(req.id, "true", Statement.Block(req.trueId), Statement.Block(req.falseId))
    doInsert(req.afterId, newStat, req.blockId)

  def addWhile(req: AddWhile): FunctionModel =
    val newStat = Statement.While(req.id, "false", Statement.Block(req.bodyId))
    doInsert(req.afterId, newStat, req.blockId)

  def addDoWhile(req: AddDoWhile): FunctionModel =
    val newStat = Statement.DoWhile(req.id, "false", Statement.Block(req.bodyId))
    doInsert(req.afterId, newStat, req.blockId)

  def addForLoop(req: AddForLoop): FunctionModel =
    val newStat = Statement.ForLoop(req.id, "i", "0", "1", "10", Statement.Block(req.bodyId))
    doInsert(req.afterId, newStat, req.blockId)

  def updateDeclare(req: UpdateDeclare): FunctionModel =
    var updatedStat: Statement.Declare = doFind(req.id).asInstanceOf[Statement.Declare]
    req.name.foreach(n => updatedStat = updatedStat.copy(name = n))
    req.tpe.foreach(t => updatedStat = updatedStat.copy(tpe = t))
    req.expr.foreach(e => updatedStat = updatedStat.copy(initValue = e))
    doUpdate(req.id, updatedStat)

  def updateAssign(req: UpdateAssign): FunctionModel =
    var updatedStat = doFind(req.id).asInstanceOf[Statement.Assign]
    req.name.foreach(n => updatedStat = updatedStat.copy(name = n))
    req.expr.foreach(e => updatedStat = updatedStat.copy(value = e))
    doUpdate(req.id, updatedStat)

  def updateOutput(req: UpdateOutput): FunctionModel =
    val newStat = Statement.Output(req.id, req.expr)
    doUpdate(req.id, newStat)

  def updateInput(req: UpdateInput): FunctionModel =
    val newStat = Statement.Input(req.id, req.name)
    doUpdate(req.id, newStat)

  def updateCall(req: UpdateCall): FunctionModel =
    val newStat = Statement.Call(req.id, req.expr)
    doUpdate(req.id, newStat)

  def updateReturn(req: UpdateReturn): FunctionModel =
    var updatedStat = doFind(req.id).asInstanceOf[Statement.Return]
    req.expr.foreach(e => updatedStat = updatedStat.copy(maybeValue = e))
    doUpdate(req.id, updatedStat)

  def updateIf(req: UpdateIf): FunctionModel =
    var updatedStat = doFind(req.id).asInstanceOf[Statement.If]
    updatedStat = updatedStat.copy(condition = req.expr)
    doUpdate(req.id, updatedStat)

  def updateWhile(req: UpdateWhile): FunctionModel =
    var updatedStat = doFind(req.id).asInstanceOf[Statement.While]
    updatedStat = updatedStat.copy(condition = req.expr)
    doUpdate(req.id, updatedStat)

  def updateDoWhile(req: UpdateDoWhile): FunctionModel =
    var updatedStat = doFind(req.id).asInstanceOf[Statement.DoWhile]
    updatedStat = updatedStat.copy(condition = req.expr)
    doUpdate(req.id, updatedStat)

  def updateForLoop(req: UpdateForLoop): FunctionModel =
    var updatedStat = doFind(req.id).asInstanceOf[Statement.ForLoop]
    req.varName.foreach(n => updatedStat = updatedStat.copy(varName = n))
    req.start.foreach(e => updatedStat = updatedStat.copy(start = e))
    req.incr.foreach(e => updatedStat = updatedStat.copy(incr = e))
    req.end.foreach(e => updatedStat = updatedStat.copy(end = e))
    doUpdate(req.id, updatedStat)

  def delete(req: Delete): FunctionModel =
    val newStats = delete(ast.statements, req.id)
    this.copy(ast = ast.copy(statements = newStats))

  /* HELPERS */
  private def doInsert(afterId: String, newStatement: Statement, blockId: String): FunctionModel =
    val newStats = insert(ast.statements, afterId, newStatement, blockId)
    this.copy(ast = ast.copy(statements = newStats))

  private def insert(
      statements: List[Statement],
      afterId: String,
      newStatement: Statement,
      blockId: String
  ): List[Statement] = {
    //println(s"insert $newStatement after $afterId in block $blockId")
    val afterStatementIdx = statements.indexWhere(_.id == afterId)
    if (afterStatementIdx >= 0) {
      val afterStatement = statements(afterStatementIdx)
      if blockId.startsWith("fun-") then return statements.patch(afterStatementIdx + 1, List(newStatement), 0)

      afterStatement match {
        case stmt: Statement.If =>
          val newStmt =
            if (stmt.trueBlock.id == blockId)
              stmt.copy(trueBlock =
                stmt.trueBlock
                  .copy(statements = stmt.trueBlock.statements.prepended(newStatement))
              )
            else
              stmt.copy(falseBlock =
                stmt.falseBlock.copy(statements = stmt.falseBlock.statements.prepended(newStatement))
              )
          statements.updated(afterStatementIdx, newStmt)
        case stmt: Statement.While =>
          val newStmt =
            if (stmt.body.id == blockId)
              stmt.copy(body = stmt.body.copy(statements = stmt.body.statements.prepended(newStatement)))
            else
              stmt
          statements.updated(afterStatementIdx, newStmt)
        case stmt: Statement.DoWhile =>
          val newStmt =
            if (stmt.body.id == blockId)
              stmt.copy(body = stmt.body.copy(statements = stmt.body.statements.prepended(newStatement)))
            else
              stmt
          statements.updated(afterStatementIdx, newStmt)
        case stmt: Statement.ForLoop =>
          val newStmt =
            if (stmt.body.id == blockId)
              stmt.copy(body = stmt.body.copy(statements = stmt.body.statements.prepended(newStatement)))
            else
              stmt
          statements.updated(afterStatementIdx, newStmt)
        case _ =>
          statements.patch(afterStatementIdx + 1, List(newStatement), 0)
      }
    } else {
      statements.map {
        case ifStatement: Statement.If =>
          ifStatement.copy(
            trueBlock = ifStatement.trueBlock
              .copy(statements = insert(ifStatement.trueBlock.statements, afterId, newStatement, blockId)),
            falseBlock = ifStatement.falseBlock.copy(statements =
              insert(ifStatement.falseBlock.statements, afterId, newStatement, blockId)
            )
          )
        case whileStatement: Statement.While =>
          whileStatement.copy(
            body = whileStatement.body.copy(statements =
              insert(whileStatement.body.statements, afterId, newStatement, blockId)
            )
          )
        case doWhileStatement: Statement.DoWhile =>
          doWhileStatement.copy(
            body = doWhileStatement.body.copy(statements =
              insert(doWhileStatement.body.statements, afterId, newStatement, blockId)
            )
          )
        case stmt: Statement.ForLoop =>
          stmt.copy(
            body = stmt.body.copy(statements = insert(stmt.body.statements, afterId, newStatement, blockId))
          )
        case simple =>
          simple
      }
    }
  }

  private def doUpdate(statementId: String, newStatement: Statement): FunctionModel =
    val newStats = update(ast.statements, statementId, newStatement)
    //println(s"OLD: ${ast.statements} \nNEW: $newStatement, \nNEW: $newStats")
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
        case ifStatement: Statement.If =>
          ifStatement.copy(
            trueBlock = ifStatement.trueBlock
              .copy(statements = update(ifStatement.trueBlock.statements, statementId, newStatement)),
            falseBlock = ifStatement.falseBlock.copy(
              statements = update(ifStatement.falseBlock.statements, statementId, newStatement)
            )
          )
        case whileStatement: Statement.While =>
          whileStatement.copy(
            body =
              whileStatement.body.copy(statements = update(whileStatement.body.statements, statementId, newStatement))
          )
        case doWhileStatement: Statement.DoWhile =>
          doWhileStatement.copy(
            body = doWhileStatement.body.copy(statements =
              update(doWhileStatement.body.statements, statementId, newStatement)
            )
          )
        case stmt: Statement.ForLoop =>
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
    import Statement.*
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

  private def findById(
      statements: List[Statement],
      statementId: String
  ): Option[Statement] =
    import Statement.*
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
