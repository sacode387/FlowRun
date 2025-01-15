package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol
import dev.sacode.flowrun.parse.Token

class PythonGenerator(val programAst: Program) extends CodeGenerator {

  override def genToken(token: Token): String = token.tpe match {
    case Token.Type.True  => "True"
    case Token.Type.False => "False"
    case Token.Type.Not   => "not"
    case Token.Type.And   => "and"
    case Token.Type.Or    => "or"
    case _                => token.text
  }

  def generate: Try[CodeGenRes] = Try {
    // TODO optionally import
    addLine("import math", "")
    addLine("from random import randrange", "")

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

    val params = function.parameters.map(p => s"${p.name}").mkString(", ")
    addEmptyLine()
    addLine(
      s"def ${function.name}($params):",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    if function.statements.length == 2 && function.tpe == Type.Void then addLine("pass", function.statements.head.id)
    decrIndent()

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
        addLine(s"${d.name} = $genValue", d.id)

      case Assign(id, name, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$name = $genValue", id)

      case Call(id, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$genValue", id)

      case Input(id, name, promptOpt) =>
        promptOpt
          .orElse {
            Option.when(programAst.config.useInputPrompt)(s"Please enter $name: ")
          } match
          case Some(prompt) => addLine(s"""$name = input("$prompt")""", id)
          case None         => addLine(s"""$name = input()""", id)

      case Output(id, value, newline) =>
        val genValue = parseGenExpr(value)
        val text =
          if newline then s"""print($genValue)"""
          else s"""print($genValue, end = '')"""
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
        addLine(s"if $genCond:", id)
        genStatement(trueBlock)
        if trueBlock.statements.isEmpty then addLine("  pass", id)
        addLine("else:", id)
        genStatement(falseBlock)
        if falseBlock.statements.isEmpty then addLine("  pass", id)

      case While(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"while $genCond:", id)
        genStatement(block)
        if block.statements.isEmpty then addLine("  pass", id)

      // https://stackoverflow.com/questions/743164/how-to-emulate-a-do-while-loop
      case DoWhile(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"while True:", id)
        genStatement(block)
        if block.statements.isEmpty then addLine("  pass", id)
        addLine(s"  if not $genCond:", id)
        addLine(s"    break", id)

      case ForLoop(id, varName, start, incr, end, block) =>
        val genStart = parseGenExpr(start)
        val genIncr = parseGenExpr(incr)
        val genEnd = parseGenExpr(end)
        addLine(s"for $varName in range($genStart, $genEnd+1, $genIncr):", id)
        genStatement(block)
        if block.statements.isEmpty then addLine("  pass", id)

      case Comment(id, text) =>
        addLine(s""" ""\" ${text} ""\" */""", id)

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs             => s"math.fabs(${argOpt(0)})"
      case Floor           => s"math.floor(${argOpt(0)})"
      case Ceil            => s"math.ceil(${argOpt(0)})"
      case RandomInteger   => s"randrange(${argOpt(0)})"
      case Sin             => s"math.sin(${argOpt(0)})"
      case Cos             => s"math.cos(${argOpt(0)})"
      case Tan             => s"math.tan(${argOpt(0)})"
      case Ln              => s"math.log(${argOpt(0)})"
      case Log10           => s"math.log10(${argOpt(0)})"
      case Log2            => s"math.log2(${argOpt(0)})"
      case Sqrt            => s"sqrt(${argOpt(0)})"
      case Pow             => s"pow(${argOpt(0)}, ${argOpt(1)})"
      case Length          => s"len(${argOpt(0)})"
      case CharAt          => s"${argOpt(0)}[${argOpt(1)}]"
      case RealToInteger   => s"int(${argOpt(0)})"
      case StringToInteger => s"int(${argOpt(0)})"
      case ReadInput       => "Console.ReadLine()"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name (${genArgs.mkString(", ")}) """.trim

}
