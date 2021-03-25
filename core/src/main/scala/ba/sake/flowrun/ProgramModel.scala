package ba.sake.flowrun

import ba.sake.flowrun.parse._, Expression.Type

final class ProgramModel(
  programAst: Program
) {
  
  var ast = programAst

  private var statementByIds = Map.empty[String, Statement]

  def addDeclare(id: String, name: String, tpe: Type)(afterId: String, blockId: String): Unit = {
    val newStat = Statement.Declare(id, name, tpe, None)
    doInsert(afterId, newStat, blockId)
  }

  def addAssign(id: String)(afterId: String, blockId: String): Unit = {
    val newStat = Statement.Assign(id, "", parseExpr(id, "null"))
    doInsert(afterId, newStat, blockId)
  }

  def addOutput(id: String)(afterId: String, blockId: String): Unit = {
    val newStat = Statement.Output(id, parseExpr(id, "\"<TODO>\""))
    doInsert(afterId, newStat, blockId)
  }

  def addIf(id: String, trueId: String, falseId: String, endId: String)(afterId: String, blockId: String): Unit = {
    val condExpr = parseExpr(id, "true")
    val newStat = Statement.If(id, condExpr, Statement.Block(trueId), Statement.Block(falseId))
    val newEndStat = Statement.BlockEnd(endId)
    
    doInsert(afterId, newEndStat, blockId) // insert END marker, so we can insert/delete properly..
    doInsert(afterId, newStat, blockId)
  }

  // TODO
  def updateDeclare(id: String, name: Option[String] = None, tpe: Option[Type] = None, expr: Option[Expression] = None): Unit = {
    var updatedStat = statementByIds(id).asInstanceOf[Statement.Declare]
    name.foreach(n => updatedStat = updatedStat.copy(name = n))
    tpe.foreach(t => updatedStat = updatedStat.copy(tpe = t))
    expr.foreach(e => updatedStat = updatedStat.copy(initValue = expr))
    doUpdate(id, updatedStat)
  }

  def updateAssign(id: String, name: Option[String] = None, expr: Option[Expression] = None): Unit = {
    var updatedStat = statementByIds(id).asInstanceOf[Statement.Assign]
    name.foreach(n => updatedStat = updatedStat.copy(name = n))
    expr.foreach(e => updatedStat = updatedStat.copy(value = e))
    doUpdate(id, updatedStat)
  }

  def updateOutput(id: String, expr: Expression): Unit = {
    val newStat = Statement.Output(id, expr)
    doUpdate(id, newStat)
  }

  def updateIf(id: String, expr: Expression): Unit = {
    var updatedStat = statementByIds(id).asInstanceOf[Statement.If]
    println(s"IF BEFORE: $updatedStat")
    updatedStat = updatedStat.copy(condition = expr)
    println(s"IF AFTER: $updatedStat")
    doUpdate(id, updatedStat)
  }

  def delete(id: String): Unit = {
    val newStats = delete(ast.statements, id)
    ast = ast.copy(statements = newStats)
    statementByIds -= id
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
    statements: Seq[Statement],
    afterId: String,
    newStatement: Statement,
    blockId: String
  ): Seq[Statement] = {
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
          statements.patch(afterStatementIdx + 1, Seq(newStatement), 0)
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
    statements: Seq[Statement],
    statementId: String,
    newStatement: Statement
  ): Seq[Statement] = {
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
    statements: Seq[Statement],
    statementId: String
  ): Seq[Statement] = {
    import Statement._
    statements.flatMap {
      case Block(_, blockStats) =>
        delete(blockStats, statementId)
      case ifStat @ If(id, expr, trueBlock, falseBlock) =>
        if statementId == id then Seq.empty
        else 
          val newIfStat = ifStat
            .copy(trueBlock = trueBlock.copy(statements = delete(trueBlock.statements, statementId)))
            .copy(falseBlock = falseBlock.copy(statements = delete(falseBlock.statements, statementId)))
          Seq(newIfStat)
      case st => // TODO: [minor] remove BlockEnd..
        Option.unless(st.id == statementId)(st)
    }
  }
}