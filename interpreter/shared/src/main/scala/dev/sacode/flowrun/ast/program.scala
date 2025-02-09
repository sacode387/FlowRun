package dev.sacode.flowrun.ast

import java.util.UUID
import ba.sake.tupson.*
import dev.sacode.flowrun.parse.parseExpr

case class Function(
    rawId: String,
    name: String,
    parameters: List[Function.Parameter] = List.empty,
    tpe: Expression.Type = Expression.Type.Void,
    statements: List[Statement] = List.empty
) derives JsonRW:

  val id = s"fun-$rawId"

  def isMain: Boolean = rawId == "main"

  def label: String =
    val title = if isMain then "main" else name
    val params = if isMain then "" else s"(${parameters.map(p => s"${p.name}").mkString(", ")})"
    s"$title$params"
  def verboseLabel: String =
    val title = if isMain then "main" else name
    val params = if isMain then "" else s"(${parameters.map(p => s"${p.name}: ${p.tpe}").mkString(", ")})"
    s"$title$params: $tpe"

object Function:
  case class Parameter(id: String, name: String, tpe: Expression.Type) derives JsonRW:
    def pretty: String = s"$name: $tpe"

case class Program(
    id: String,
    name: String,
    config: FlowRunConfig,
    main: Function,
    functions: List[Function] = List.empty,
    version: String = "0.3",
    revision: Int = 1 // autoincrement revision on any change
) derives JsonRW:

  def allFunctions: List[Function] =
    functions.prepended(main)

  def hasInputs: Boolean =
    allStmts.exists(_.isInstanceOf[Statement.Input]) ||
      usesFunction(PredefinedFunction.ReadInput)

  def usesFunction(f: PredefinedFunction): Boolean =
    allExprs.exists { exprStr =>
      val expr = parseExpr("dummy", exprStr)
      expr.collectAtoms.exists {
        case fc: Atom.FunctionCall => fc.name == f.name
        case _                     => false
      }
    }

  private def allStmts: Seq[Statement] = {
    def getStatements(s: Statement): Seq[Statement] = s match {
      case stmt: Statement.Block =>
        stmt.statements.flatMap(getStatements)
      case stmt: Statement.If =>
        getStatements(stmt.trueBlock) ++ getStatements(stmt.falseBlock)
      case stmt: Statement.While =>
        getStatements(stmt.body)
      case stmt: Statement.DoWhile =>
        getStatements(stmt.body)
      case stmt: Statement.ForLoop =>
        getStatements(stmt.body)
      case _ => Seq(s)
    }
    allFunctions.flatMap(_.statements).flatMap(getStatements)
  }

  private def allExprs: Seq[String] = {
    def getExprs(s: Statement): Seq[String] = s match {
      case stmt: Statement.Return =>
        stmt.maybeValue.toSeq
      case stmt: Statement.Declare =>
        stmt.initValue.toSeq
      case stmt: Statement.Assign =>
        Seq(stmt.value)
      case stmt: Statement.Call =>
        Seq(stmt.value)
      case stmt: Statement.Output =>
        Seq(stmt.value)
      case stmt: Statement.Block =>
        stmt.statements.flatMap(getExprs)
      case stmt: Statement.If =>
        Seq(stmt.condition) ++ getExprs(stmt.trueBlock) ++ getExprs(stmt.falseBlock)
      case stmt: Statement.While =>
        Seq(stmt.condition) ++ getExprs(stmt.body)
      case stmt: Statement.DoWhile =>
        Seq(stmt.condition) ++ getExprs(stmt.body)
      case stmt: Statement.ForLoop =>
        Seq(stmt.start) ++ Seq(stmt.incr) ++ Seq(stmt.end) ++ getExprs(stmt.body)
      case _ => Seq.empty
    }
    allStmts.flatMap(getExprs)
  }

final case class FlowRunConfig(
    lang: String,
    showFunctions: Boolean = false,
    showGenCode: Boolean = false,
    showDebugVars: Boolean = true,
    showIoBtns: Boolean = true,
    useInputPrompt: Boolean = false,
    echoEnteredValue: Boolean = false
) derives JsonRW

object FlowRunConfig:
  val default = FlowRunConfig("java", true, true)
