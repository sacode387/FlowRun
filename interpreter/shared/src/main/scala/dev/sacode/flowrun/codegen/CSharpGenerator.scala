package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class CSharpGenerator(val programAst: Program) extends CodeGenerator {

  protected override def matrixGet(name: String, index1: String, index2: String): String =
    s"${name}[${index1}, ${index2}]"

  def generate: Try[CodeGenRes] = Try {

    addLine(s"using System;")
    addLine(s"class ${programAst.name.toIdentifier} {", programAst.main.id)

    incrIndent()
    if programAst.usesFunction(PredefinedFunction.RandomInteger) then
      addLine("static Random random = new Random();", "")
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
      "static void Main() {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("}", function.statements.head.id)

    symTab.exitScope()
  }

  private def genFunction(function: Function): Unit = {
    symTab.enterScope(function.id, function.name)

    val params = function.parameters.map(p => s"${genType(p.tpe)} ${p.name}").mkString(", ")
    addEmptyLine()
    addLine(
      s"static ${genType(function.tpe)} ${function.name}($params) {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("}", function.id)

    symTab.exitScope()
  }

  private def genStatement(stmt: Statement): Unit = {
    import Statement._
    stmt match {
      case _: Begin => // noop
      case d: Declare =>
        val key = SymbolKey(d.name, Symbol.Kind.Variable, d.id)
        symTab.add(d.id, key, d.tpe, None)
        val initExpr = generateExpr(d)
        addLine(s"${genType(d.tpe)} ${d.name} = $initExpr;", d.id)

      case Assign(id, name, value) =>
        val genName = parseGenExpr(name)
        val genValue = parseGenExpr(value)
        addLine(s"$genName = $genValue;", id)

      case Call(id, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$genValue;", id)

      case Input(id, name, promptOpt) =>
        promptOpt
          .orElse {
            Option.when(programAst.config.useInputPrompt)(s"Please enter $name: ")
          }
          .foreach { prompt =>
            addLine(s"""Console.Write("$prompt");""", id)
          }
        val baseName = name.takeWhile(_ != '[')
        val symOpt = Try(symTab.getSymbolVar("", baseName)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        val genName = parseGenExpr(name)
        addLine(s"$genName = $readFun;", id)

      case Output(id, value, newline) =>
        val genValue = parseGenExpr(value)
        val text =
          if newline then s"Console.WriteLine($genValue);"
          else s"Console.Write($genValue);"
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
        addLine(s"for (int $varName = $genStart; $varName <= $genEnd; $varName += $genIncr) {", id)
        genStatement(block)
        addLine("}", id)

      case Comment(id, text) =>
        addLine(s"/* ${text} */", id)
    }
  }

  private def generateExpr(d: Statement.Declare) =
    d.tpe match {
      case Type.Void    => d.initValue.map(parseGenExpr).getOrElse("{}")
      case Type.Integer => parseGenExpr(d.initValue.getOrElse("0"))
      case Type.Real    => parseGenExpr(d.initValue.getOrElse("0.0"))
      case Type.String  => parseGenExpr(d.initValue.getOrElse(""" "" """.trim))
      case Type.Boolean => parseGenExpr(d.initValue.getOrElse("false".trim))
      case Type.IntegerArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" new int[${dim1}] ".trim
      case Type.RealArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" new double[${dim1}] ".trim
      case Type.StringArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" new string[${dim1}] ".trim
      case Type.BooleanArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" new bool[${dim1}] ".trim
      case Type.IntegerMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" new int[${dim1}, ${dim2}] ".trim
      case Type.RealMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" new double[${dim1}, ${dim2}] ".trim
      case Type.StringMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" new string[${dim1}, ${dim2}] ".trim
      case Type.BooleanMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" new bool[${dim1}, ${dim2}] ".trim
    }

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs             => s"Math.abs(${argOpt(0)})"
      case Floor           => s"Math.floor(${argOpt(0)})"
      case Ceil            => s"Math.ceil(${argOpt(0)})"
      case RandomInteger   => s"random.Next(${argOpt(0)})"
      case Sin             => s"Math.sin(${argOpt(0)})"
      case Cos             => s"Math.cos(${argOpt(0)})"
      case Tan             => s"Math.tan(${argOpt(0)})"
      case Ln              => s"Math.log(${argOpt(0)})"
      case Log10           => s"Math.log10(${argOpt(0)})"
      case Log2            => s"Math.log10(${argOpt(0)})/Math.log10(2)"
      case Sqrt            => s"Math.sqrt(${argOpt(0)})"
      case Pow             => s"Math.pow(${argOpt(0)}, ${argOpt(1)})"
      case Length          => s"${argOpt(0)}.length()"
      case CharAt          => s"${argOpt(0)}.charAt(${argOpt(1)})"
      case RealToInteger   => s"(int)${argOpt(0)}"
      case StringToInteger => s"Convert.ToInt32(${argOpt(0)})"
      case ReadInput       => "Console.ReadLine()"
      case ClearOutput     => "Console.Clear()"
      case NumRows         => s"${argOpt(0)}.GetLength(0)"
      case NumCols         => s"${argOpt(0)}.GetLength(1)"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

  /* TYPE */
  private def genType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void          => "void"
      case Integer       => "int"
      case IntegerArray  => "int[]"
      case IntegerMatrix => "int[,]"
      case Real          => "double"
      case RealArray     => "double[]"
      case RealMatrix    => "double[,]"
      case String        => "string"
      case StringArray   => "string[]"
      case StringMatrix  => "string[,]"
      case Boolean       => "bool"
      case BooleanArray  => "bool[]"
      case BooleanMatrix => "bool[,]"

  /* OTHER */
  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => "Console.ReadLine()"
    case Some(tpe) =>
      tpe match
        case Type.Integer | Type.IntegerArray | Type.IntegerMatrix => "Convert.ToInt32(Console.ReadLine())"
        case Type.Real | Type.RealArray | Type.RealMatrix          => "Convert.ToDouble(Console.ReadLine())"
        case Type.Boolean | Type.BooleanArray | Type.BooleanMatrix => "Convert.ToBoolean(Console.ReadLine())"
        case _                                                     => "Console.ReadLine()"

}
