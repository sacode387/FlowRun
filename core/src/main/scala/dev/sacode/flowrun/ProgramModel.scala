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

  // I'm too lazy to make this a request parameter :/
  var currentFunctionId = MainFunId

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
      statements = List(Statement.Begin(false), Statement.Return(AST.newId))
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
          val newParams = params.map((n, t) => (n, Expression.Type.valueOf(t)))
          updatedFun = updatedFun.copy(parameters = newParams)
        }
        updatedFun
      else f
    }
    ast = ast.copy(functions = newFunctions)
    flowrunChannel := FlowRun.Event.FunctionUpdated

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

  def addWhile(req: AddWhile): Unit =
    update(_.addWhile(req))

  def addDoWhile(req: AddDoWhile): Unit =
    update(_.addDoWhile(req))

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

  def updateReturn(req: UpdateReturn): Unit =
    update(_.updateReturn(req))

  def updateIf(req: UpdateIf): Unit =
    update(_.updateIf(req))

  def updateWhile(req: UpdateWhile): Unit =
    update(_.updateWhile(req))

  def updateDoWhile(req: UpdateDoWhile): Unit =
    update(_.updateDoWhile(req))

  def delete(req: Delete): Unit =
    update(_.delete(req))

  def findStatement(stmtId: String): Statement =
    FunctionModel(currentFunction).doFind(stmtId)

  private def update(transform: FunctionModel => FunctionModel): Unit = {
    val newFunction = transform(FunctionModel(currentFunction)).ast
    if currentFunction.isMain then ast = ast.copy(main = newFunction)
    else
      ast.functions.indexWhere(_.id == currentFunctionId) match
        case -1 =>
          println(s"Oops, function $currentFunctionId does not exist...")
        case idx =>
          val newFunctions = ast.functions.updated(idx, newFunction)
          ast = ast.copy(functions = newFunctions)

    flowrunChannel := FlowRun.Event.SyntaxSuccess
  }
}


object ProgramModel:
  import dev.sacode.flowrun.Expression.Type
  
  val MainFunId = "fun-main"

  enum Request:
    case Delete(id: String)
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
    case UpdateFunction(
        id: String,
        name: Option[String] = None,
        tpe: Option[Type] = None,
        parameters: Option[List[(String, String)]] = None
    )
end ProgramModel

case class FunctionModel(
    ast: Function
) {
  import ProgramModel.Request.*

  def addDeclare(req: AddDeclare): FunctionModel =
    val newStat = Statement.Declare(req.id, req.name, req.tpe, None)
    doInsert(req.afterId, newStat, req.blockId)

  def addAssign(req: AddAssign): FunctionModel =
    val newStat = Statement.Assign(req.id, "x", "?")
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
    val newStat =
      Statement.If(req.id, "true", Statement.Block(req.trueId), Statement.Block(req.falseId))
    doInsert(req.afterId, newStat, req.blockId)

  def addWhile(req: AddWhile): FunctionModel =
    val newStat =
      Statement.While(req.id, "true", Statement.Block(req.bodyId))
    doInsert(req.afterId, newStat, req.blockId)

  def addDoWhile(req: AddDoWhile): FunctionModel =
    val newStat =
      Statement.DoWhile(req.id, "true", Statement.Block(req.bodyId))
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
    var updatedStat: Statement.If = doFind(req.id).asInstanceOf[Statement.If]
    updatedStat = updatedStat.copy(condition = req.expr)
    doUpdate(req.id, updatedStat)

  def updateWhile(req: UpdateWhile): FunctionModel =
    var updatedStat: Statement.While = doFind(req.id).asInstanceOf[Statement.While]
    updatedStat = updatedStat.copy(condition = req.expr)
    doUpdate(req.id, updatedStat)

  def updateDoWhile(req: UpdateDoWhile): FunctionModel =
    var updatedStat: Statement.DoWhile = doFind(req.id).asInstanceOf[Statement.DoWhile]
    updatedStat = updatedStat.copy(condition = req.expr)
    doUpdate(req.id, updatedStat)

  def delete(req: Delete): FunctionModel =
    val newStats = delete(ast.statements, req.id)
    this.copy(ast = ast.copy(statements = newStats))

  /* HELPERS */
  private def doInsert(afterId: String, newStatement: Statement, blockId: String): FunctionModel =
    val newStats =
      if ast.statements.isEmpty then List(newStatement)
      //else if afterId == "beginId" || afterId.startsWith("fun-") then
      //  ast.statements.prepended(newStatement)
      else insert(ast.statements, afterId, newStatement, blockId)
    this.copy(ast = ast.copy(statements = newStats))

  private def insert(
      statements: List[Statement],
      afterId: String,
      newStatement: Statement,
      blockId: String
  ): List[Statement] = {
    println(s"insert $newStatement after $afterId in $blockId")
    val afterStatementIdx = statements.indexWhere(_.id == afterId)
    if (afterStatementIdx >= 0) {
      val afterStatement = statements(afterStatementIdx)
      afterStatement match {
        case ifStatement: Statement.If =>
          val newIfStatement =
            if (ifStatement.trueBlock.id == blockId)
              ifStatement.copy(trueBlock =
                ifStatement.trueBlock
                  .copy(statements = ifStatement.trueBlock.statements.prepended(newStatement))
              )
            else
              ifStatement.copy(falseBlock =
                ifStatement.falseBlock.copy(statements =
                  ifStatement.falseBlock.statements.prepended(newStatement)
                )
              )
          statements.updated(afterStatementIdx, newIfStatement)
        case whileStatement: Statement.While =>
          val newWhileStatement =
            if (whileStatement.body.id == blockId)
              whileStatement.copy(body =
                whileStatement.body
                  .copy(statements = whileStatement.body.statements.prepended(newStatement))
              )
            else
              whileStatement
          statements.updated(afterStatementIdx, newWhileStatement)
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
            trueBlock = ifStatement.trueBlock.copy(statements =
              update(ifStatement.trueBlock.statements, statementId, newStatement)
            ),
            falseBlock = ifStatement.falseBlock.copy(statements =
              update(ifStatement.falseBlock.statements, statementId, newStatement)
            )
          )
        case whileStatement: Statement.While =>
          whileStatement.copy(
            body = whileStatement.body.copy(statements =
              update(whileStatement.body.statements, statementId, newStatement)
            )
          )
        case doWhileStatement: Statement.DoWhile =>
          doWhileStatement.copy(
            body = doWhileStatement.body.copy(statements =
              update(doWhileStatement.body.statements, statementId, newStatement)
            )
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
      case ifStat @ If(id, expr, trueBlock, falseBlock) =>
        if statementId == id then List.empty
        else
          val newIfStat = ifStat
            .copy(trueBlock =
              trueBlock.copy(statements = delete(trueBlock.statements, statementId))
            )
            .copy(falseBlock =
              falseBlock.copy(statements = delete(falseBlock.statements, statementId))
            )
          List(newIfStat)
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
      case stmt =>
        Option.when(stmt.id == statementId)(stmt)
    }.headOption

}
