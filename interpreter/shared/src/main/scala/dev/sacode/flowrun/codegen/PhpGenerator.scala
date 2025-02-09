package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class PhpGenerator(val programAst: Program) extends CodeGenerator {

  override def identPrefix: String = "$"

  def generate: Try[CodeGenRes] = Try {

    addLine("<?php")
    genMain()
    programAst.functions.foreach(genFunction)
    addLine("?>")

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

    val params = function.parameters.map(p => s"$$${p.name}").mkString(", ")
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
      case d: Declare =>
        val key = SymbolKey(d.name, Symbol.Kind.Variable, d.id)
        symTab.add(d.id, key, d.tpe, None)
        val initValue = d.initValue.getOrElse(defaultValue(d.tpe))
        val genValue = parseGenExpr(initValue)
        addLine(s"$$${d.name} = $genValue;", d.id)

      case Assign(id, name, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$$$name = $genValue;", id)

      case Call(id, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$genValue;", id)

      case Input(id, name, promptOpt) =>
        promptOpt
          .orElse {
            Option.when(programAst.config.useInputPrompt)(s"Please enter $name: ")
          } match
          case Some(prompt) => addLine(s"$$$name = readline($prompt);", id)
          case None         => addLine(s"$$$name = readline();", id)

      case Output(id, value, newline) =>
        val genValue = parseGenExpr(value)
        val text =
          if newline then s"print($genValue);"
          else s"print($genValue);"
        addLine(text, id)

      case Block(_, statements) =>
        incrIndent()
        statements.foreach(genStatement)
        decrIndent()

      case Return(id, maybeValue) =>
        maybeValue.foreach { value =>
          val genValue = parseGenExpr(value)
          addLine(s"return $genValue;", id)
        }

      case If(id, condition, trueBlock, falseBlock) =>
        val genCond = parseGenExpr(condition)
        addLine(s"if ($genCond) {", id)
        genStatement(trueBlock)
        addLine("} else {", id)
        genStatement(falseBlock)
        addLine("}", id)

      case While(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"while ($genCond) {", id)
        genStatement(block)
        addLine("}", id)

      case DoWhile(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"do {", id)
        genStatement(block)
        addLine(s"} while ($genCond);", id)

      case ForLoop(id, varName, start, incr, end, block) =>
        val genStart = parseGenExpr(start)
        val genIncr = parseGenExpr(incr)
        val genEnd = parseGenExpr(end)
        addLine(s"for (let $varName = $genStart; $varName <= $genEnd; $varName += $genIncr) {", id)
        genStatement(block)
        addLine("}", id)

      case Comment(id, text) =>
        addLine(s"/* ${text} */", id)

  private def defaultValue(tpe: Type): String = tpe match {
    case Type.Void    => ""
    case Type.Boolean => "false"
    case Type.Integer => "0"
    case Type.Real    => "0.0"
    case Type.String  => """ "" """.trim
    case _            => sys.error(s"No default value for array or matrix")
  }

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs             => s"abs(${argOpt(0)})"
      case Floor           => s"floor(${argOpt(0)})"
      case Ceil            => s"ceil(${argOpt(0)})"
      case RandomInteger   => s"rand(0,${argOpt(0)})"
      case Sin             => s"sin(${argOpt(0)})"
      case Cos             => s"cos(${argOpt(0)})"
      case Tan             => s"tan(${argOpt(0)})"
      case Ln              => s"log(${argOpt(0)})"
      case Log10           => s"log10(${argOpt(0)})"
      case Log2            => s"log2(${argOpt(0)})"
      case Sqrt            => s"sqrt(${argOpt(0)})"
      case Pow             => s"pow(${argOpt(0)}, ${argOpt(1)})"
      case Length          => s"strlen(${argOpt(0)})"
      case CharAt          => s"${argOpt(0)}[${argOpt(1)}]"
      case RealToInteger   => argOpt(0)
      case StringToInteger => s""" intval(${argOpt(0)}) """.trim
      case ReadInput       => "readline()"
      case ClearOutput     => s""" echo chr(27).chr(91).'H'.chr(27).chr(91).'J'; """
      case NumRows         => s"count(${argOpt(0)})"
      case NumCols         => s"count(${argOpt(0)}[0])"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

}
