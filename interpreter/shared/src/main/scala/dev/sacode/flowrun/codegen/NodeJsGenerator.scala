package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class NodeJsGenerator(override val programAst: Program) extends CodeGenerator {

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

  private def genStatement(stmt: Statement): Unit = {
    import Statement._
    stmt match
      case _: Begin => // noop
      case d: Declare =>
        val key = SymbolKey(d.name, Symbol.Kind.Variable, d.id)
        symTab.add(d.id, key, d.tpe, None)
        val initExpr = generateExpr(d)
        addLine(s"let ${d.name} = $initExpr;", d.id)

      case Assign(id, name, value) =>
        val genName = parseGenExpr(name)
        val genValue = parseGenExpr(value)
        addLine(s"$genName = $genValue;", id)

      case Call(id, value) =>
        addLine(s"$value;", id)

      case Input(id, name, promptOpt) =>
        promptOpt
          .orElse {
            Option.when(programAst.config.useInputPrompt)(s"Please enter $name: ")
          }
          .foreach { prompt =>
            addLine(s"""process.stdout.write("$prompt");""", id)
          }
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
        addLine(s"for (let $varName = $start; $varName <= $end; $varName += $incr) {", id)
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
        s" Array(${dim1}).fill(0) ".trim
      case Type.RealArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array(${dim1}).fill(0) ".trim
      case Type.StringArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array(${dim1}).fill('') ".trim
      case Type.BooleanArray =>
        val dim1 = parseGenExpr(d.lengthValue1)
        s" Array(${dim1}).fill(false) ".trim
      case Type.IntegerMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array(${dim1}).fill(Array(${dim2}).fill(0)) ".trim
      case Type.RealMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array(${dim1}).fill(Array(${dim2}).fill(0)) ".trim
      case Type.StringMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array(${dim1}).fill(Array(${dim2}).fill('')) ".trim
      case Type.BooleanMatrix =>
        val dim1 = parseGenExpr(d.lengthValue1)
        val dim2 = parseGenExpr(d.lengthValue2)
        s" Array(${dim1}).fill(Array(${dim2}).fill(false)) ".trim
    }

  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => "line"
    case Some(tpe) =>
      tpe match
        case Type.Integer => "line"
        case Type.Real    => "line"
        case Type.Boolean => "line"
        case _            => "line"

  import PredefinedFunction.*

  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")

    PredefinedFunction.withName(name).get match {
      case Abs             => s"Math.abs(${argOpt(0)})"
      case Floor           => s"Math.floor(${argOpt(0)})"
      case Ceil            => s"Math.ceil(${argOpt(0)})"
      case RandomInteger   => s"Math.floor(Math.random()*${argOpt(0)})"
      case Sin             => s"Math.sin(${argOpt(0)})"
      case Cos             => s"Math.cos(${argOpt(0)})"
      case Tan             => s"Math.tan(${argOpt(0)})"
      case Ln              => s"Math.log(${argOpt(0)})"
      case Log10           => s"Math.log10(${argOpt(0)})"
      case Log2            => s"Math.log2(${argOpt(0)})"
      case Sqrt            => s"Math.sqrt(${argOpt(0)})"
      case Pow             => s"Math.pow(${argOpt(0)}, ${argOpt(1)})"
      case Length          => s"${argOpt(0)}.length"
      case CharAt          => s"${argOpt(0)}.charAt(${argOpt(1)})"
      case RealToInteger   => argOpt(0) // ??
      case StringToInteger => s"parseInt(${argOpt(0)})"
      case ReadInput       => "line"
      case ClearOutput     => "process.stdout.write('\\x1Bc')"
      case NumRows         => s"${argOpt(0)}.length"
      case NumCols         => s"${argOpt(0)}[0].length"
    }
  }

  override def funCall(name: String, genArgs: List[String]): String =
    s""" $name(${genArgs.mkString(", ")}) """.trim

}
