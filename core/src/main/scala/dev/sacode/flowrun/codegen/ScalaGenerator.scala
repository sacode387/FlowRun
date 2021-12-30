package dev.sacode.flowrun.codegen

import dev.sacode.flowrun.Program
import dev.sacode.flowrun.Statement
import dev.sacode.flowrun.Expression

class ScalaGenerator(programAst: Program) {

  def generate: String = {

    val statements = programAst.main.statements.map(getStatement)
    s"""
    package flowrun

    import scala.io.StdIn

    @main def ${programAst.main.name}(): Unit = {
      ${statements.mkString("\n")}
    }
    """
  }

  private def getStatement(stmt: Statement): String =
    import Statement._
    stmt match
      case Declare(_, name, tpe, maybeInitValue) =>
        val initValue = maybeInitValue.map(v => s" = $v").getOrElse("")
        s"var $name: ${getType(tpe)}$initValue"
      case Assign(_, name, value) =>
        s"$name = $value"
      case Call(_, value) =>
        value
      case Input(_, name) =>
        s"$name = StdIn.readLine()"
      case Output(_, value)      => s"println($value)"
      case Block(_, statements)  => statements.map(getStatement).mkString("\n")
      case Return(_, maybeValue) => maybeValue.getOrElse("")
      case If(_, condition, trueBlock, falseBlock) =>
        s"""|
            |if ($condition) {
            |  ${getStatement(trueBlock)}
            |} else {
            |  ${getStatement(falseBlock)}
            |}""".stripMargin.trim

  private def getType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void    => "Unit"
      case Integer => "Long"
      case Real    => "Double"
      case String  => "String"
      case Boolean => "Boolean"

}
