package dev.sacode.flowrun.codegen

import scala.util.Try

import scala.collection.mutable
import scala.util.Try
import reactify.*
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.indented
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol
import scala.collection.mutable.ListBuffer

trait CodeGenerator {

  def programAst: Program

  def generate: Try[CodeGenRes]

  private val indentAmount = 2
  protected  var indent = 0

  private val dummyChannel = Channel[FlowRun.Event]
  protected val symTab = SymbolTable(dummyChannel) // needed for types of vars

  private var lineNum = 1
  protected var lines = ListBuffer.empty[String]
  protected var stmtLineNums: mutable.Map[String, List[Int]] = mutable.Map.empty.withDefaultValue(List.empty)

  protected def addLine(text: String, stmtId: String): Unit =
    lines += text.indented(indent)
    if !stmtId.trim.isEmpty then
      val lineNums = stmtLineNums.getOrElse(stmtId, List.empty)
      stmtLineNums.put(stmtId, lineNums.appended(lineNum))
    lineNum += 1

  protected def addEmptyLine(): Unit =
    lines += ""
    lineNum += 1

  protected def incrIndent(): Unit =
    indent += indentAmount
  protected def decrIndent(): Unit =
    indent -= indentAmount

  protected def defaultValue(tpe: Type): String = tpe match {
    case Type.Void => ""
    case Type.Boolean => "false"
    case Type.Integer => "0"
    case Type.Real => "0.0"
    case Type.String => """ "" """.trim
  }
}

case class CodeGenRes(
    lines: List[String],
    stmtLineNums: Map[String, List[Int]]
)
