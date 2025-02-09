package dev.sacode.flowrun.codegen

import dev.sacode.flowrun.ast.*
import dev.sacode.flowrun.ast.Expression.Type
import dev.sacode.flowrun.eval.{Symbol, SymbolKey}
import dev.sacode.flowrun.toIdentifier

import scala.util.Try

class ScalaGenerator(val programAst: Program) extends CodeGenerator {

  protected override def arrayGet(name: String, index: String): String =
    s"${name}(${index})"

  protected override def matrixGet(name: String, index1: String, index2: String): String =
    s"${name}(${index1})(${index2})"

  def generate: Try[CodeGenRes] = Try {

    if programAst.hasInputs then
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

    addLine("}", function.statements.last.id)

    symTab.exitScope()
  }

  private def genFunction(function: Function): Unit = {
    symTab.enterScope(function.id, function.name)

    val params = function.parameters.map(p => s"${p.name}: ${genType(p.tpe)}").mkString(", ")
    addEmptyLine()
    addLine(
      s"def ${function.name}($params): ${genType(function.tpe)} = {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("}", function.id)

    symTab.exitScope()
  }

  private def genStatement(stmt: Statement): Unit = {
    import Statement.*
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
          addLine(genValue, id)
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
        addLine(s"for ($varName <- $genStart to $genEnd by $genIncr) {", id)
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
        s" Array.ofDim[Int](${dim1}) ".trim
      case Type.RealArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array.ofDim[Double](${dim1}) ".trim
      case Type.StringArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array.ofDim[String](${dim1}) ".trim
      case Type.BooleanArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array.ofDim[Boolean](${dim1}) ".trim
      case Type.IntegerMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.ofDim[Int](${dim1}, ${dim2}) ".trim
      case Type.RealMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.ofDim[Double](${dim1}, ${dim2}) ".trim
      case Type.StringMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.ofDim[String](${dim1}, ${dim2}) ".trim
      case Type.BooleanMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.ofDim[Boolean](${dim1}, ${dim2}) ".trim
    }

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs             => s"${argOpt(0)}.abs"
      case Floor           => s"${argOpt(0)}.floor"
      case Ceil            => s"${argOpt(0)}.ceil"
      case RandomInteger   => s"scala.util.Random.nextInt(${argOpt(0)})"
      case Sin             => s"Math.sin(${argOpt(0)})"
      case Cos             => s"Math.cos(${argOpt(0)})"
      case Tan             => s"Math.tan(${argOpt(0)})"
      case Ln              => s"Math.log(${argOpt(0)})"
      case Log10           => s"Math.log10(${argOpt(0)})"
      case Log2            => s"Math.log10(${argOpt(0)})/Math.log10(2)"
      case Sqrt            => s"scala.math.sqrt(${argOpt(0)})"
      case Pow             => s"scala.math.pow(${argOpt(0)}, ${argOpt(1)})"
      case Length          => s"${argOpt(0)}.length"
      case CharAt          => s"${argOpt(0)}.charAt(${argOpt(1)})"
      case RealToInteger   => s"${argOpt(0)}.toInt"
      case StringToInteger => s"${argOpt(0)}.toInt"
      case ReadInput       => "StdIn.readLine()"
      case NumRows         => s"${argOpt(0)}.length"
      case NumCols         => s"${argOpt(0)}(0).length"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

  private def genType(tpe: Expression.Type): String =
    import Expression.Type
    import Type.*
    tpe match
      case Void          => "Unit"
      case Integer       => "Int"
      case IntegerArray  => "Array[Int]"
      case IntegerMatrix => "Array[Array[Int]]"
      case Real          => "Double"
      case RealArray     => "Array[Double]"
      case RealMatrix    => "Array[Array[Double]]"
      case String        => "String"
      case StringArray   => "Array[String]"
      case StringMatrix  => "Array[Array[String]]"
      case Boolean       => "Boolean"
      case BooleanArray  => "Array[Boolean]"
      case BooleanMatrix => "Array[Array[Boolean]]"

  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => "StdIn.readLine()"
    case Some(tpe) =>
      tpe match
        case Type.Integer | Type.IntegerArray | Type.IntegerMatrix => "StdIn.readInt()"
        case Type.Real | Type.RealArray | Type.RealMatrix          => "StdIn.readDouble()"
        case Type.Boolean | Type.BooleanArray | Type.BooleanMatrix => "StdIn.readBoolean()"
        case _                                                     => "StdIn.readLine()"

}
