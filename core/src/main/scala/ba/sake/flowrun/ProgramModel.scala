package ba.sake.flowrun

import scala.scalajs.js
import ba.sake.flowrun.parse.parseExpr

class ProgramModel(
  programAst: Program
) {
  import ProgramModel.Request.*

  var ast = programAst

  // I'm too lazy to make this a request parameter :/
  var currentFunctionName = "main"

  def currentFunction: Function =
    if currentFunctionName == "main" then ast.main
    else ast.functions.find(_.name == currentFunctionName).get

  def addFunction(fun: Function): Unit =
    val newFunctions = ast.functions.appended(fun)
    ast = ast.copy(functions = newFunctions)
  
  def deleteFunction(name: String): Unit =
    val newFunctions = ast.functions.filterNot(_.name == name)
    ast = ast.copy(functions = newFunctions)

  /* per-function */
  def addDeclare(req: AddDeclare): Unit =
    update(_.addDeclare(req))

  def addAssign(req: AddAssign): Unit =
    update(_.addAssign(req))

  def addOutput(req: AddOutput): Unit =
    update(_.addOutput(req))

  def addInput(req: AddInput): Unit =
    update(_.addInput(req))
  
  def addCall(req: AddCall): Unit =
    update(_.addCall(req))

  def addIf(req: AddIf): Unit =
    update(_.addIf(req))

  def updateDeclare(req: UpdateDeclare): Unit =
    update(_.updateDeclare(req))

  def updateAssign(req: UpdateAssign): Unit =
    update(_.updateAssign(req))

  def updateOutput(req: UpdateOutput): Unit =
    update(_.updateOutput(req))

  def updateInput(req: UpdateInput): Unit =
    update(_.updateInput(req))
  
  def updateCall(req: UpdateCall): Unit =
    update(_.updateCall(req))

  def updateIf(req: UpdateIf): Unit =
    update(_.updateIf(req))

  def delete(req: Delete): Unit =
    update(_.delete(req))
  
  private def update(transform: FunctionModel => FunctionModel): Unit = {
    val newFunction = transform(FunctionModel(currentFunction)).ast
    if currentFunction.isMain then
      ast = ast.copy(main = newFunction)
    else
      ast.functions.indexWhere(_.name == currentFunctionName) match
        case -1 =>
          println(s"Oops, function $currentFunctionName does not exist...")
        case idx =>
          val newFunctions = ast.functions.updated(idx, newFunction)
          ast = ast.copy(functions = newFunctions)
  }
}

case class FunctionModel(
  ast: Function
) {
  import ProgramModel.Request.*

  def addDeclare(req: AddDeclare): FunctionModel =
    val newStat = Statement.Declare(req.id, req.name, req.tpe, None)
    doInsert(req.afterId, newStat, req.blockId)

  def addAssign(req: AddAssign): FunctionModel =
    val newStat = Statement.Assign(req.id, "", "\"\"")
    doInsert(req.afterId, newStat, req.blockId)

  def addOutput(req: AddOutput): FunctionModel =
    val newStat = Statement.Output(req.id, "\"output\"")
    doInsert(req.afterId, newStat, req.blockId)

  def addInput(req: AddInput): FunctionModel =
    val newStat = Statement.Input(req.id, "")
    doInsert(req.afterId, newStat, req.blockId)

  def addCall(req: AddCall): FunctionModel =
    val newStat = Statement.Call(req.id, "")
    doInsert(req.afterId, newStat, req.blockId)

  def addIf(req: AddIf): FunctionModel =
    val newStat = Statement.If(req.id, "\"\"", Statement.Block(req.trueId), Statement.Block(req.falseId))
    doInsert(req.afterId, newStat, req.blockId)

    // insert END marker, so we can load AST to flowchart
    //val newEndStat = Statement.BlockEnd(req.endId)
   // doInsert(req.afterId, newEndStat, req.blockId)

  def updateDeclare(req: UpdateDeclare): FunctionModel =
    var updatedStat: Statement.Declare = doFind(req.id).asInstanceOf[Statement.Declare]
    req.name.foreach(n => updatedStat = updatedStat.copy(name = n))
    req.tpe.foreach(t => updatedStat = updatedStat.copy(tpe = t))
    req.expr.foreach(e => updatedStat = updatedStat.copy(initValue = e))
    doUpdate(req.id, updatedStat)

  def updateAssign(req: UpdateAssign): FunctionModel =
    var updatedStat: Statement.Assign = doFind(req.id).asInstanceOf[Statement.Assign]
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

  def updateIf(req: UpdateIf): FunctionModel =
    var updatedStat: Statement.If = doFind(req.id).asInstanceOf[Statement.If]
    updatedStat = updatedStat.copy(condition = req.expr)
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
    val afterStatementIdx = statements.indexWhere(_.id == afterId)
    if (afterStatementIdx >= 0) {
      val afterStatement = statements(afterStatementIdx)
      afterStatement match {
        case ifStatement: Statement.If =>
          val newIfStatement =
            if (ifStatement.trueBlock.id == blockId) ifStatement.copy(trueBlock =
              ifStatement.trueBlock.copy(statements =
                ifStatement.trueBlock.statements.prepended(newStatement)
              )
            ) else ifStatement.copy(falseBlock =
              ifStatement.falseBlock.copy(statements =
                ifStatement.falseBlock.statements.prepended(newStatement)
              )
            )
          statements.updated(afterStatementIdx, newIfStatement)
        case _ =>
          statements.patch(afterStatementIdx + 1, List(newStatement), 0)
      }
    } else {
      statements.map {
        case ifStatement: Statement.If =>
          ifStatement.copy(
            trueBlock = ifStatement.trueBlock.copy(statements =
              insert(ifStatement.trueBlock.statements, afterId, newStatement, blockId)
            ),
            falseBlock = ifStatement.falseBlock.copy(statements =
              insert(ifStatement.falseBlock.statements, afterId, newStatement, blockId)
            )
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
        throw RuntimeException(s"Statement type mismatch. Existing: ${existingStatement.getClass} New: ${newStatement.getClass}")
      }
      statements.updated(statementIdx, newStatement)
    } else {
      statements.map {
        case ifStatement: Statement.If =>
          val newIfStatement = ifStatement.copy(
            trueBlock = ifStatement.trueBlock.copy(statements =
              update(ifStatement.trueBlock.statements, statementId, newStatement)
            ),
            falseBlock = ifStatement.falseBlock.copy(statements =
              update(ifStatement.falseBlock.statements, statementId, newStatement)
            )
          )
          newIfStatement
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
      case ifStat @ If(id, expr, trueBlock, falseBlock) =>
        if statementId == id then List.empty
        else 
          val newIfStat = ifStat
            .copy(trueBlock = trueBlock.copy(statements = delete(trueBlock.statements, statementId)))
            .copy(falseBlock = falseBlock.copy(statements = delete(falseBlock.statements, statementId)))
          List(newIfStat)
      case st => // TODO: [minor] remove BlockEnd..?
        Option.unless(st.id == statementId)(st)
    }
  }

  private def doFind(statementId: String): Statement =
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
        Option.when(statementId == id)(ifStat)
          .orElse(findById(trueBlock.statements, statementId))
          .orElse(findById(falseBlock.statements, statementId))
      case stmt =>
        Option.when(stmt.id == statementId)(stmt)
    }.headOption

}

object ProgramModel:
  import ba.sake.flowrun.Expression.Type
  enum Request:
    case Delete(id: String)
    case AddDeclare(id: String, name: String, tpe: Type, afterId: String, blockId: String)
    case AddAssign(id: String, afterId: String, blockId: String)
    case AddOutput(id: String, afterId: String, blockId: String)
    case AddInput(id: String, afterId: String, blockId: String)
    case AddCall(id: String, afterId: String, blockId: String)
    case AddIf(id: String, trueId: String, falseId: String, endId: String, afterId: String, blockId: String)
    case UpdateDeclare(id: String, name: Option[String] = None, tpe: Option[Type] = None, expr: Option[Option[String]] = None)
    case UpdateAssign(id: String, name: Option[String] = None, expr: Option[String] = None)
    case UpdateOutput(id: String, expr: String)
    case UpdateInput(id: String, name: String)
    case UpdateCall(id: String, expr: String)
    case UpdateIf(id: String, expr: String)
