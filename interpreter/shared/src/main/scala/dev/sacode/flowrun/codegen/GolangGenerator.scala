package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class GolangGenerator(val programAst: Program) extends CodeGenerator {

  def generate: Try[CodeGenRes] = Try {

    addLine("package main")
    addLine("""import "fmt"""")
    addLine("""import "strconv"""")

    if programAst.hasInputs then
      addLine("""import "bufio"""", programAst.main.id)
      addLine("""import "os"""", programAst.main.id)
      addEmptyLine()
      addLine("""var reader = bufio.NewReader(os.Stdin)""", programAst.main.id)

    genMain()
    programAst.functions.foreach(genFunction)

    CodeGenRes(lines.toList, stmtLineNums.toMap)
  }

  private def genMain(): Unit = {
    val function = programAst.main
    symTab.enterScope(function.id, function.name)

    addEmptyLine()
    addLine(
      "func main() {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("}", function.statements.last.id)

    symTab.exitScope()
  }

  private def genFunction(function: Function): Unit = {
    symTab.enterScope(function.id, function.name)

    val params = function.parameters.map(p => s"${p.name} ${genType(p.tpe)}").mkString(", ")
    addEmptyLine()
    addLine(
      s"func ${function.name}($params) ${genType(function.tpe)} {",
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
        val initValueExpr = parseGenExpr(initValue)
        addLine(s"var $name ${genType(tpe)} = $initValueExpr", id)

      case Assign(id, name, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$name = $genValue", id)

      case Call(id, value) =>
        val genValue = parseGenExpr(value)
        addLine(genValue, id)

      case Input(id, name, promptOpt) =>
        val prompt = promptOpt.getOrElse(s"Please enter $name: ")
        addLine(s"""fmt.Print("$prompt")""", id)

        val symOpt = Try(symTab.getSymbolVar("", name)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        addLine(s"$name = $readFun", id)

      case Output(id, value, newline) =>
        val genValue = parseGenExpr(value)
        val text =
          if newline then s"fmt.Println($genValue)"
          else s"fmt.Print($genValue)"
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
        addLine(s"if ($genCond) {", id)
        genStatement(trueBlock)
        addLine("} else {", id)
        genStatement(falseBlock)
        addLine("}", id)

      case While(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"for $genCond {", id)
        genStatement(block)
        addLine("}", id)

      case DoWhile(id, condition, block) =>
        val genCond = parseGenExpr(condition)
        addLine(s"do {", id)
        genStatement(block)
        addLine(s"} while ($genCond)", id)

      case ForLoop(id, varName, start, incr, end, block) =>
        val genStart = parseGenExpr(start)
        val genIncr = parseGenExpr(incr)
        val genEnd = parseGenExpr(end)
        addLine(s"for $varName <- $genStart to $genEnd by $genIncr {", id)
        genStatement(block)
        addLine("}", id)

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs             => s"${argOpt(0)}.abs"
      case Floor           => s"${argOpt(0)}.floor"
      case Ceil            => s"${argOpt(0)}.ceil"
      case RandomInteger   => s"scala.util.Random(${argOpt(0)})"
      case Sin             => s"Math.sin(${argOpt(0)})"
      case Cos             => s"Math.cos(${argOpt(0)})"
      case Tan             => s"Math.tan(${argOpt(0)})"
      case Ln              => s"Math.log(${argOpt(0)})"
      case Log10           => s"Math.log10(${argOpt(0)})"
      case Log2            => s"Math.log10(${argOpt(0)})/Math.log10(2)"
      case Sqrt            => s"Math.sqrt(${argOpt(0)})"
      case Pow             => s"Math.pow(${argOpt(0)}, ${argOpt(1)})"
      case Length          => s"${argOpt(0)}.length"
      case CharAt          => s"${argOpt(0)}.charAt(${argOpt(1)})"
      case RealToInteger   => s"${argOpt(0)}.toInt"
      case StringToInteger => s"strconv.Itoa(${argOpt(0)})"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

  private def genType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void    => ""
      case Integer => "int"
      case Real    => "float64"
      case String  => "string"
      case Boolean => "bool"

  // TODO wtf golang..
  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => """reader.ReadString("")"""
    case Some(tpe) =>
      tpe match
        case Type.Integer => """strconv.Atoi(reader.ReadString(""))"""
        case Type.Real    => """strconv.ParseFloat(reader.ReadString(""), 64)"""
        case Type.Boolean => """strconv.ParseBool(reader.ReadString(""))"""
        case _            => """reader.ReadString("")"""

}
