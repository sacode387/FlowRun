package ba.sake.flowrun

import scala.scalajs.js, js.annotation._
import ba.sake.flowrun.parse.parseExpr
import ba.sake.flowrun.exec.Request._

@JSExportTopLevel("ProgramModel", Module.Exec)
final class ProgramModel(
  programAst: Program
) {
  
  var ast = programAst

  private var statementByIds = Map.empty[String, Statement]

  def addDeclare(req: AddDeclare): Unit = {
    val newStat = Statement.Declare(req.id, req.name, req.tpe, None)
    doInsert(req.afterId, newStat, req.blockId)
  }

  def addAssign(req: AddAssign): Unit = {
    val newStat = Statement.Assign(req.id, "", parseExpr(req.id, "null"))
    doInsert(req.afterId, newStat, req.blockId)
  }

  def addOutput(req: AddOutput): Unit = {
    val newStat = Statement.Output(req.id, parseExpr(req.id, "\"TODO\""))
    doInsert(req.afterId, newStat, req.blockId)
  }

  def addInput(req: AddInput): Unit = {
    val newStat = Statement.Input(req.id, "", parseExpr(req.id, "null"))
    doInsert(req.afterId, newStat, req.blockId)
  }

  def addIf(req: AddIf): Unit = {
    val condExpr = parseExpr(req.id, "\"\"")
    val newStat = Statement.If(req.id, condExpr, Statement.Block(req.trueId), Statement.Block(req.falseId))
    val newEndStat = Statement.BlockEnd(req.endId)
    
    doInsert(req.afterId, newEndStat, req.blockId) // insert END marker, so we can insert/delete properly..
    doInsert(req.afterId, newStat, req.blockId)
  }

  // TODO
  def updateDeclare(req: UpdateDeclare): Unit = {
    var updatedStat = statementByIds(req.id).asInstanceOf[Statement.Declare]
    req.name.foreach(n => updatedStat = updatedStat.copy(name = n))
    req.tpe.foreach(t => updatedStat = updatedStat.copy(tpe = t))
    req.expr.foreach(e => updatedStat = updatedStat.copy(initValue = req.expr))
    doUpdate(req.id, updatedStat)
  }

  def updateAssign(req: UpdateAssign): Unit = {
    var updatedStat = statementByIds(req.id).asInstanceOf[Statement.Assign]
    req.name.foreach(n => updatedStat = updatedStat.copy(name = n))
    req.expr.foreach(e => updatedStat = updatedStat.copy(value = e))
    doUpdate(req.id, updatedStat)
  }

  def updateOutput(req: UpdateOutput): Unit = {
    val newStat = Statement.Output(req.id, req.expr)
    doUpdate(req.id, newStat)
  }

  def updateIf(req: UpdateIf): Unit = {
    var updatedStat = statementByIds(req.id).asInstanceOf[Statement.If]
    //println(s"IF BEFORE: $updatedStat")
    updatedStat = updatedStat.copy(condition = req.expr)
    //println(s"IF AFTER: $updatedStat")
    doUpdate(req.id, updatedStat)
  }

  def delete(req: Delete): Unit = {
    val newStats = delete(ast.statements, req.id)
    ast = ast.copy(statements = newStats)
    statementByIds -= req.id
  }

  /* HELPERS */
  private def doInsert(afterId: String, newStatement: Statement, blockId: String): Unit = {
    println(s"INSERT: $afterId, $blockId, $newStatement")
    val newStats = insert(ast.statements, afterId, newStatement, blockId)
    ast = ast.copy(statements = newStats)
    statementByIds += newStatement.id -> newStatement
    println(statementByIds)
  }

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
          statementByIds += newIfStatement.id -> newIfStatement
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
  
  private def doUpdate(statementId: String, newStatement: Statement): Unit = {
    val newStats = update(ast.statements, statementId, newStatement)
    ast = ast.copy(statements = newStats)
    statementByIds += newStatement.id -> newStatement
  }

  private def update(
    statements: List[Statement],
    statementId: String,
    newStatement: Statement
  ): List[Statement] = {
    val statementIdx = statements.indexWhere(_.id == statementId)
    if (statementIdx >= 0) {
      val existingStatement = statements(statementIdx)
      if (existingStatement.getClass != newStatement.getClass) {
        throw new RuntimeException(s"Statement type mismatch. Existing: ${existingStatement.getClass} New: ${newStatement.getClass}")
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
          statementByIds += newIfStatement.id -> newIfStatement
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
    import Statement._
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
      case st => // TODO: [minor] remove BlockEnd..
        Option.unless(st.id == statementId)(st)
    }
  }
}