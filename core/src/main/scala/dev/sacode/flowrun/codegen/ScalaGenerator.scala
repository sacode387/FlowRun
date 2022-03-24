package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class ScalaGenerator(override val programAst: Program) extends CodeGenerator {

  def generate: Try[CodeGenRes] = Try {

    addLine("import scala.io.StdIn", programAst.main.id)
    addEmptyLine()
    addLine(s"object ${programAst.name.toIdentifier} {", programAst.main.id)
    
    incrIndent()
    genMain()
    programAst.functions.foreach(genFunction)
    decrIndent()

    addLine("}", programAst.main.id)

    CodeGenRes(lines.toList, stmtLineNums.toMap)
  }

  private def genMain(): Unit = {
    val function = programAst.main
    symTab.enterScope(function.id, function.name)

    addEmptyLine()
    addLine(
      "def main(args: Array[String]): Unit = {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("}", function.id)

    symTab.exitScope()
  }

  private def genFunction(function: Function): Unit = {
    symTab.enterScope(function.id, function.name)

    val params = function.parameters.map(p => s"${p.name}: ${getType(p.tpe)}").mkString(", ")
    addEmptyLine()
    addLine(
      s"def ${function.name}($params): ${getType(function.tpe)} = {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("}", function.id)

    symTab.exitScope()
  }

  private def genStatement(stmt: Statement): Unit =
    import Statement._
    stmt match
      case _: Begin => // noop

      case Declare(id, name, tpe, maybeInitValue) =>
        val key = SymbolKey(name, Symbol.Kind.Variable, id)
        symTab.add(id, key, tpe, None)
        val initValue = maybeInitValue.getOrElse(defaultValue(tpe))
        addLine(s"var $name: ${getType(tpe)} = $initValue", id)

      case Assign(id, name, value) =>
        addLine(s"$name = $value", id)

      case Call(id, value) =>
        addLine(value, id)

      case Input(id, name, promptOpt) =>
        val prompt = promptOpt.getOrElse(s"Please enter $name: ")
        addLine(s"""print("$prompt")""", id)

        val symOpt = Try(symTab.getSymbolVar("", name)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        addLine(s"$name = StdIn.$readFun", id)

      case Output(id, value, newline) =>
        val text =
          if newline then s"println($value)"
          else s"print($value)"
        addLine(text, id)

      case Block(_, statements) =>
        incrIndent()
        statements.foreach(genStatement)
        decrIndent()

      case Return(id, maybeValue) =>
        maybeValue.foreach { value =>
          addLine(value, id)
        }

      case If(id, condition, trueBlock, falseBlock) =>
        addLine(s"if ($condition) {", id)
        genStatement(trueBlock)
        addLine("} else {", id)
        genStatement(falseBlock)
        addLine("}", id)

      case While(id, condition, block) =>
        addLine(s"while ($condition) {", id)
        genStatement(block)
        addLine("}", id)

      case DoWhile(id, condition, block) =>
        addLine(s"do {", id)
        genStatement(block)
        addLine(s"} while ($condition)", id)

      case ForLoop(id, varName, start, incr, end, block) =>
        addLine(s"for ($varName <- $start to $end) {", id)
        genStatement(block)
        addLine("}", id)

  private def getType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void    => "Unit"
      case Integer => "Int"
      case Real    => "Double"
      case String  => "String"
      case Boolean => "Boolean"

  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => "readLine()"
    case Some(tpe) =>
      tpe match
        case Type.Integer => "readInt()"
        case Type.Real    => "readDouble()"
        case Type.Boolean => "readBoolean()"
        case _            => "readLine()"

}
