package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class RubyGenerator(val programAst: Program) extends CodeGenerator {

  def generate: Try[CodeGenRes] = Try {

    programAst.functions.foreach(genFunction)

    addEmptyLine()
    genMain()

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

    // we put an underscore to avoid naming args..
    val params = function.parameters.map(_.name).mkString(", ")
    addEmptyLine()
    addLine(
      s"def ${function.name}($params)",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("end", function.id)

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
        val initValueExpr = parseGenExpr(initValue)
        addLine(s"$name = $initValueExpr", id)

      case Assign(id, name, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$name = $genValue", id)

      case Call(id, value) =>
        val genValue = parseGenExpr(value)
        addLine(genValue, id)

      case Input(id, name, promptOpt) =>
        val prompt = promptOpt.getOrElse(s"Please enter $name: ")
        addLine(s"""print("$prompt")""", id)

        val symOpt = Try(symTab.getSymbolVar("", name)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        addLine(s"$name = $readFun", id)

      case Output(id, value, newline) =>
        val genValue = parseGenExpr(value)
        val text =
          if newline then s"puts($genValue)"
          else s"""print($genValue)"""
        addLine(text, id)

      case Block(_, statements) =>
        incrIndent()
        statements.foreach(genStatement)
        decrIndent()

      case Return(id, maybeValue) =>
        maybeValue.foreach { value =>
          val genValue = parseGenExpr(value)
          addLine(s"return $genValue", id)
        }

      case If(id, condition, trueBlock, falseBlock) =>
        val genCond = parseGenExpr(condition)
        addLine(s"if $genCond then", id)
        genStatement(trueBlock)
        addLine("else", id)
        genStatement(falseBlock)
        addLine("end", id)

      case While(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"while $genCond do", id)
        genStatement(block)
        addLine("end", id)

      case DoWhile(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"repeat {", id)
        genStatement(block)
        addLine(s"} while $genCond", id)

      case ForLoop(id, varName, start, incr, end, block) =>
        val genStart = parseGenExpr(start)
        val genIncr = parseGenExpr(incr)
        val genEnd = parseGenExpr(end)
        addLine(s"for $varName in stride(from: $genStart, through: $genEnd, by: $genIncr) {", id)
        genStatement(block)
        addLine("end", id)

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs           => s"${argOpt(0)}.abs"
      case Floor         => s"${argOpt(0)}.floor"
      case Ceil          => s"${argOpt(0)}.ceil"
      case RandomInteger => s"scala.util.Random(${argOpt(0)})"
      case Sin           => s"Math.sin(${argOpt(0)})"
      case Cos           => s"Math.cos(${argOpt(0)})"
      case Tan           => s"Math.tan(${argOpt(0)})"
      case Ln            => s"Math.log(${argOpt(0)})"
      case Log10         => s"Math.log10(${argOpt(0)})"
      case Log2          => s"Math.log10(${argOpt(0)})/Math.log10(2)"
      case Length        => s"${argOpt(0)}.length"
      case CharAt        => s"${argOpt(0)}.charAt(${argOpt(1)})"
      case RealToInteger => s"${argOpt(0)}.toInt"
      case StringToInteger => s"${argOpt(0)}.to_i"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

  private def readFunction(tpeOpt: Option[Type]): String =
    "$stdin.read"

}
