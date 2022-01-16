package dev.sacode.flowrun.codegen

import dev.sacode.flowrun.Program
import dev.sacode.flowrun.Function
import dev.sacode.flowrun.Statement
import dev.sacode.flowrun.Expression
import dev.sacode.flowrun.indented

class ScalaGenerator(programAst: Program) {

  def generate: String = {

    val statements = programAst.main.statements.map(s => genStatement(s, 2)).filterNot(_.trim.isEmpty)
    val functions = programAst.functions.map(genFunction)
    s"""|package flowrun
        |
        |import scala.io.StdIn
        |
        |@main def ${programAst.main.name}(): Unit = {
        |
        |${statements.mkString("\n")}
        |}
        |
        |${functions.mkString("\n")}
        |
        |""".stripMargin
  }

  private def genFunction(function: Function): String = {
    val statements = function.statements.map(s => genStatement(s, 2)).filterNot(_.trim.isEmpty)
    val params = function.parameters.map(p => s"${p.name}: ${getType(p.tpe)}").mkString(", ")
    s"""|def ${function.name}($params): ${getType(function.tpe)} = {
        |${statements.mkString("\n")}
        |}
        |""".stripMargin
  }

  private def genStatement(stmt: Statement, indent: Int): String =
    import Statement._
    stmt match
      case _: Begin =>
        ""
      case Declare(_, name, tpe, maybeInitValue) =>
        val initValue = maybeInitValue.map(v => s" = $v").getOrElse("")
        s"var $name: ${getType(tpe)}$initValue".indented(indent)
      case Assign(_, name, value) =>
        s"$name = $value".indented(indent)
      case Call(_, value) =>
        value.indented(indent)
      case Input(_, name) =>
        s"$name = StdIn.readLine()".indented(indent)
      case Output(_, value) =>
        s"println($value)".indented(indent)
      case Block(_, statements) =>
        statements.map(s => genStatement(s, indent)).mkString("\n").indented(indent)
      case Return(_, maybeValue) =>
        maybeValue.getOrElse("").indented(indent)
      case If(_, condition, trueBlock, falseBlock) =>
        s"""|if ($condition) {
            |  ${genStatement(trueBlock, indent + 2)}
            |} else {
            |  ${genStatement(falseBlock, indent + 2)}
            |}""".stripMargin.trim.indented(indent)
      case While(_, condition, block) =>
        s"""|while ($condition) {
            |  ${genStatement(block, indent + 2)}
            |}""".stripMargin.trim.indented(indent)
      case DoWhile(_, condition, block) =>
        s"""|while {
            |  ${genStatement(block, indent + 2)}
            |  ($condition)
            |} do ()""".stripMargin.trim.indented(indent)

  private def getType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void    => "Unit"
      case Integer => "Int"
      case Real    => "Double"
      case String  => "String"
      case Boolean => "Boolean"

}
