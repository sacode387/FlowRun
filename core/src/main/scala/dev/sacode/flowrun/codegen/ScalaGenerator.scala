package dev.sacode.flowrun.codegen

import reactify.*
import dev.sacode.flowrun.Program
import dev.sacode.flowrun.Function
import dev.sacode.flowrun.Statement
import dev.sacode.flowrun.Expression
import dev.sacode.flowrun.indented
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol
import dev.sacode.flowrun.Expression.Type
import scala.util.Try

class Scala2Generator(programAst: Program) {

  private val indent = 2

  private val dummyChannel = Channel[FlowRun.Event]
  private val symTab = SymbolTable(dummyChannel) // needed for types of vars

  def generate: Try[String] = Try {

    val statements = programAst.main.statements.map(genStatement).filterNot(_.trim.isEmpty)
    val functions = programAst.functions.map(genFunction)
    s"""|package flowrun
        |
        |import scala.io.StdIn
        |
        |object ${programAst.main.name} extends App {
        |
        |${statements.mkString("\n")}
        |
        |${functions.mkString("\n")}
        |}
        |""".stripMargin
  }

  private def genFunction(function: Function): String = {
    symTab.enterScope(function.name)
    val statements = function.statements.map(genStatement).filterNot(_.trim.isEmpty)
    val params = function.parameters.map(p => s"${p.name}: ${getType(p.tpe)}").mkString(", ")
    symTab.exitScope()
    s"""|def ${function.name}($params): ${getType(function.tpe)} = {
        |${statements.mkString("\n")}
        |}
        |""".stripMargin.indented(indent)
  }

  private def genStatement(stmt: Statement): String =
    import Statement._
    stmt match
      case _: Begin =>
        ""
      case Declare(id, name, tpe, maybeInitValue) =>
        val key = SymbolKey(name, Symbol.Kind.Variable, id)
        symTab.add(id, key, tpe, None)
        val initValue = maybeInitValue.map(v => s" = $v").getOrElse(" = _")
        s"var $name: ${getType(tpe)}$initValue".indented(indent)
      case Assign(_, name, value) =>
        s"$name = $value".indented(indent)
      case Call(_, value) =>
        value.indented(indent)
      case Input(id, name) =>
        val key = SymbolKey(name, Symbol.Kind.Variable, id)
        val sym = symTab.getSymbol(id, key)
        println(sym)
        // TODO try-catch and then resolve.. :)
        val readFun = sym.tpe match
          case Type.Integer => "readInt"
          case Type.Real    => "readDouble"
          case Type.Boolean => "readBoolean"
          case _            => "readLine"
        s"$name = StdIn.$readFun()".indented(indent)
      case Output(_, value) =>
        s"println($value)".indented(indent)
      case Block(_, statements) =>
        statements.map(genStatement).mkString("\n")
      case Return(_, maybeValue) =>
        maybeValue.getOrElse("").indented(indent)
      case If(_, condition, trueBlock, falseBlock) =>
        s"""|if ($condition) {
            |${genStatement(trueBlock)}
            |} else {
            |${genStatement(falseBlock)}
            |}""".stripMargin.trim.indented(indent)
      case While(_, condition, block) =>
        s"""|while ($condition) {
            |${genStatement(block)}
            |}""".stripMargin.trim.indented(indent)
      case DoWhile(_, condition, block) =>
        s"""|do {
            |${genStatement(block)}
            |} while ($condition)""".stripMargin.trim.indented(indent)

  private def getType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void    => "Unit"
      case Integer => "Int"
      case Real    => "Double"
      case String  => "String"
      case Boolean => "Boolean"

}
