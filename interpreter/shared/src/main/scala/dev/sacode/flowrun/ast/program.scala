package dev.sacode.flowrun.ast

import java.util.UUID
import ba.sake.tupson.*

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
  def verboseLabel =
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

  def hasInputs: Boolean = {
    val allStmts = allFunctions.flatMap(_.statements).flatMap {
      case Statement.Block(_, blockStats) =>
        blockStats
      case stat @ Statement.If(id, expr, trueBlock, falseBlock) =>
        trueBlock.statements ++ falseBlock.statements
      case stat @ Statement.While(id, expr, body) =>
        body.statements
      case stat @ Statement.DoWhile(id, expr, body) =>
        body.statements
      case stat: Statement.ForLoop =>
        stat.body.statements
      case st =>
        List(st)
    }
    allStmts.exists(_.isInstanceOf[Statement.Input])
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
