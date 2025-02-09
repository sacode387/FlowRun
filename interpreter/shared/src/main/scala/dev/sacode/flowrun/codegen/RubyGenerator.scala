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

  private def genStatement(stmt: Statement): Unit = {
    import Statement._
    stmt match
      case _: Begin => // noop
      case d: Declare =>
        val key = SymbolKey(d.name, Symbol.Kind.Variable, d.id)
        symTab.add(d.id, key, d.tpe, None)
        val initExpr = generateExpr(d)
        addLine(s"${d.name} = $initExpr", d.id)

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

      case Comment(id, text) =>
        addLine(
          s"""|=begin
              |${text}
              |=end
              |""".stripMargin,
          id
        )
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
        s" Array.new(${dim1}, 0) ".trim
      case Type.RealArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array.new(${dim1}, 0.0) ".trim
      case Type.StringArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array.new(${dim1}, '') ".trim
      case Type.BooleanArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array.new(${dim1}, false) ".trim
      case Type.IntegerMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.new(${dim1}) { Array.new(${dim2}, 0) } ".trim
      case Type.RealMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.new(${dim1}) { Array.new(${dim2}, 0.0) } ".trim
      case Type.StringMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.new(${dim1}) { Array.new(${dim2}, '') } ".trim
      case Type.BooleanMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array.new(${dim1}) { Array.new(${dim2}, false) } ".trim
    }

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs             => s"${argOpt(0)}.abs"
      case Floor           => s"${argOpt(0)}.floor"
      case Ceil            => s"${argOpt(0)}.ceil"
      case RandomInteger   => s"rand(${argOpt(0)})"
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
      case StringToInteger => s"${argOpt(0)}.to_i"
      case ReadInput       => "$stdin.read"
      case ClearOutput     => s""" system "clear" || system "cls" """
      case NumRows         => s"${argOpt(0)}.length"
      case NumCols         => s"${argOpt(0)}[0].length"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

  private def readFunction(tpeOpt: Option[Type]): String =
    "$stdin.read"

}
