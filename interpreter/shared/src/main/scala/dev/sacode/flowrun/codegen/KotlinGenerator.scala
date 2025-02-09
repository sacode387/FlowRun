package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class KotlinGenerator(val programAst: Program) extends CodeGenerator {

  def generate: Try[CodeGenRes] = Try {

    addLine("import kotlin.math.*")
    addEmptyLine()
    genMain()
    programAst.functions.foreach(genFunction)

    CodeGenRes(lines.toList, stmtLineNums.toMap)
  }

  private def genMain(): Unit = {
    val function = programAst.main
    symTab.enterScope(function.id, function.name)

    addLine(
      "fun main() {",
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

    val params = function.parameters.map(p => s"${p.name}: ${genType(p.tpe)}").mkString(", ")
    addEmptyLine()
    addLine(
      s"fun ${function.name}($params): ${genType(function.tpe)} {",
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
    stmt match
      case _: Begin => // noop
      case d: Declare =>
        val key = SymbolKey(d.name, Symbol.Kind.Variable, d.id)
        symTab.add(d.id, key, d.tpe, None)
        val initExpr = generateExpr(d)
        addLine(s"var ${d.name}: ${genType(d.tpe)} = $initExpr", d.id)

      case Assign(id, name, value) =>
        val genName = parseGenExpr(name)
        val genValue = parseGenExpr(value)
        addLine(s"$genName = $genValue", id)

      case Call(id, value) =>
        val genValue = parseGenExpr(value)
        addLine(genValue, id)

      case Input(id, name, promptOpt) =>
        promptOpt
          .orElse {
            Option.when(programAst.config.useInputPrompt)(s"Please enter $name: ")
          }
          .foreach { prompt =>
            addLine(s"""print("$prompt")""", id)
          }
        val baseName = name.takeWhile(_ != '[')
        val symOpt = Try(symTab.getSymbolVar("", baseName)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        val genName = parseGenExpr(name)
        addLine(s"$genName = $readFun", id)

      case Output(id, value, newline) =>
        val genValue = parseGenExpr(value)
        val text =
          if newline then s"println($genValue)"
          else s"print($genValue)"
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
        addLine(s"while ($genCond) {", id)
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
        addLine(s"for ($varName in $genStart..$genEnd step $genIncr) {", id)
        genStatement(block)
        addLine("}", id)

      case Comment(id, text) =>
        addLine(s"/* ${text} */", id)
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
        s" Array<Int>(${dim1}) { 0 } ".trim
      case Type.RealArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array<Double>(${dim1}) { 0.0 } ".trim
      case Type.StringArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s""" Array<String>(${dim1}) { "" } """.trim
      case Type.BooleanArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array<Boolean>(${dim1}) { false } ".trim
      case Type.IntegerMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array(${dim1}) { Array<Int>(${dim2}) { 0 } } ".trim
      case Type.RealMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array(${dim1}) { Array<Double>(${dim2}) { 0.0 } } ".trim
      case Type.StringMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s""" Array(${dim1}) { Array<String>(${dim2}) { "" } } """.trim
      case Type.BooleanMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array(${dim1}) { Array<Boolean>(${dim2}) { false } } ".trim
    }

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs           => s"abs(${argOpt(0)})"
      case Floor         => s"floor(${argOpt(0)})"
      case Ceil          => s"ceil(${argOpt(0)})"
      case RandomInteger => s"(0 until ${argOpt(0)}).random()"
      case Sin           => s"sin(${argOpt(0)})"
      case Cos           => s"cos(${argOpt(0)})"
      case Tan           => s"tan(${argOpt(0)})"
      case Ln            => s"log(${argOpt(0)})"
      case Log10         => s"log10(${argOpt(0)})"
      case Log2          => s"log10(${argOpt(0)})/log10(2)"
      case Sqrt          => s"sqrt(${argOpt(0)})"
      case Pow           => s"pow(${argOpt(0)}, ${argOpt(1)})"
      case Length        => s"${argOpt(0)}.length()"
      case CharAt        => s"${argOpt(0)}.charAt(${argOpt(1)})"
      case RealToInteger => s"(int)${argOpt(0)}"
      case StringToInteger =>
        s"""try { Integer.parseInt(${argOpt(0)}) } catch (NumberFormatException e) { 0 }"""
      case ReadInput   => "readLine()"
      case ClearOutput => """print("\u001b[H\u001b[2J")"""
      case NumRows     => s"${argOpt(0)}.size"
      case NumCols     => s"${argOpt(0)}(1).size"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

  private def genType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void          => "Unit"
      case Integer       => "Int"
      case IntegerArray  => "Int[]"
      case IntegerMatrix => "Int[][]"
      case Real          => "Double"
      case RealArray     => "Double[]"
      case RealMatrix    => "Double[][]"
      case String        => "String"
      case StringArray   => "String[]"
      case StringMatrix  => "String[][]"
      case Boolean       => "Boolean"
      case BooleanArray  => "Boolean[]"
      case BooleanMatrix => "Boolean[][]"

  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => "readLine()"
    case Some(tpe) =>
      tpe match
        case Type.Integer | Type.IntegerArray | Type.IntegerMatrix => "readLine()!!.toInt()"
        case Type.Real | Type.RealArray | Type.RealMatrix          => "readLine()!!.toDouble()"
        case Type.Boolean | Type.BooleanArray | Type.BooleanMatrix => "readLine()!!.toBoolean()"
        case _                                                     => "readLine()"

}
