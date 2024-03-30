package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class NodeJsGenerator(override val programAst: Program) extends JavascriptGenerator(programAst) {

  override def generate: Try[CodeGenRes] = Try {

    if programAst.hasInputs then addLine("const readline = require('readline');", "")

    genMain()
    programAst.functions.foreach(genFunction)

    CodeGenRes(lines.toList, stmtLineNums.toMap)
  }

  private def genMain(): Unit = {
    val function = programAst.main
    symTab.enterScope(function.id, function.name)

    function.statements.foreach(genStatement)

    symTab.exitScope()
  }

  private def genFunction(function: Function): Unit = {
    symTab.enterScope(function.id, function.name)

    val params = function.parameters.map(p => s"${p.name}").mkString(", ")
    addEmptyLine()
    addLine(
      s"function ${function.name}($params) {",
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
        addLine(s"let $name = $initValue;", id)

      case Assign(id, name, value) =>
        addLine(s"$name = $value;", id)

      case Call(id, value) =>
        addLine(s"$value;", id)

      case Input(id, name, promptOpt) =>
        val prompt = promptOpt.getOrElse(s"Please enter $name: ")
        addLine(s"""process.stdout.write("$prompt");""", id)

        val symOpt = Try(symTab.getSymbolVar("", name)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        addLine(s"$name = readline.$readFun;", id)

      case Output(id, value, newline) =>
        val text =
          if newline then s"console.log($value);"
          else s"process.stdout.write($value);"
        addLine(text, id)

      case Block(_, statements) =>
        incrIndent()
        statements.foreach(genStatement)
        decrIndent()

      case Return(id, maybeValue) =>
        maybeValue.foreach { value =>
          addLine(s"return $value;", id)
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
        addLine(s"} while ($condition);", id)

      case ForLoop(id, varName, start, incr, end, block) =>
        addLine(s"for (let $varName = $start; i <= $end; i += $incr) {", id)
        genStatement(block)
        addLine("}", id)

      case Comment(id, text) =>
        addLine(s"/* ${text} */", id)

  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => "line"
    case Some(tpe) =>
      tpe match
        case Type.Integer => "line"
        case Type.Real    => "line"
        case Type.Boolean => "line"
        case _            => "line"

}
