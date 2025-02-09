package dev.sacode.flowrun.codegen

import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.{Expression, *}
import Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class CGenerator(val programAst: Program) extends CodeGenerator {

  def generate: Try[CodeGenRes] = Try {

    addLine("#include <stdbool.h>")
    addLine("#include <string.h>")
    addLine("#include <stdio.h>")

    genMain()
    programAst.functions.foreach(genFunction)

    CodeGenRes(lines.toList, stmtLineNums.toMap)
  }

  private def genMain(): Unit = {
    val function = programAst.main
    symTab.enterScope(function.id, function.name)

    addEmptyLine()
    addLine(
      "int main() {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    addLine("return 0;")
    decrIndent()

    addLine("}", function.statements.head.id)

    symTab.exitScope()
  }

  private def genFunction(function: Function): Unit = {
    symTab.enterScope(function.id, function.name)

    val params = function.parameters.map(p => s"${genType(p.tpe)} ${p.name}").mkString(", ")
    addEmptyLine()
    addLine(
      s"${genType(function.tpe)} ${function.name}($params) {",
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
        /*
        int arr[5] = {0};
        int m[5][5] = {{0}};
         */
        val generatedLine = d.tpe match {
          case Type.Void => d.initValue.map(parseGenExpr).getOrElse("{}")
          case Type.Integer =>
            val initExpr = parseGenExpr(d.initValue.getOrElse("0"))
            s"int ${d.name} = $initExpr;"
          case Type.Real =>
            val initExpr = parseGenExpr(d.initValue.getOrElse("0.0"))
            s"double ${d.name} = $initExpr;"
          case Type.String =>
            val initExpr = parseGenExpr(d.initValue.getOrElse(""" "" """.trim))
            s"char ${d.name}[] = $initExpr;"
          case Type.Boolean =>
            val initExpr = parseGenExpr(d.initValue.getOrElse("false".trim))
            s"bool ${d.name} = $initExpr;"
          case Type.IntegerArray =>
            val dim1 = parseGenExpr(d.lengthValue1)
            s"""|int ${d.name}[${dim1}];
                |for (int i = 0; i < ${dim1}; i++) {
                |  ${d.name}[i] = 0;
                |}
                |""".stripMargin
          case Type.RealArray =>
            val dim1 = parseGenExpr(d.lengthValue1)
            s"""|double ${d.name}[${dim1}];
                |for (int i = 0; i < ${dim1}; i++) {
                |  ${d.name}[i] = 0.0;
                |}
                |""".stripMargin
          case Type.StringArray =>
            val dim1 = parseGenExpr(d.lengthValue1)
            s"""|char ${d.name}[${dim1}][100];
                |for (int i = 0; i < ${dim1}; i++) {
                |  strcpy(${d.name}[i], "");
                |}
                |""".stripMargin
          case Type.BooleanArray =>
            val dim1 = parseGenExpr(d.lengthValue1)
            s"""|bool ${d.name}[${dim1}];
                |for (int i = 0; i < ${dim1}; i++) {
                |  ${d.name}[i] = false;
                |}
                |""".stripMargin
          case Type.IntegerMatrix =>
            val dim1 = parseGenExpr(d.lengthValue1)
            val dim2 = parseGenExpr(d.lengthValue2)
            s"""|int ${d.name}[${dim1}];
                |for (int i = 0; i < ${dim1}; i++) {
                |  for (int j = 0; j < ${dim2}; j++) {
                |    ${d.name}[i][j] = 0;
                |  }
                |}
                |""".stripMargin
          case Type.RealMatrix =>
            val dim1 = parseGenExpr(d.lengthValue1)
            val dim2 = parseGenExpr(d.lengthValue2)
            s"""|double ${d.name}[${dim1}];
                |for (int i = 0; i < ${dim1}; i++) {
                |  for (int j = 0; j < ${dim2}; j++) {
                |    ${d.name}[i][j] = 0.0;
                |  }
                |}
                |""".stripMargin
          case Type.StringMatrix =>
            val dim1 = parseGenExpr(d.lengthValue1)
            val dim2 = parseGenExpr(d.lengthValue2)
            s"""|char ${d.name}[${dim1}][100];
                |for (int i = 0; i < ${dim1}; i++) {
                |  for (int j = 0; j < ${dim2}; j++) {
                |    strcpy(${d.name}[i][j], "");
                |  }
                |}
                |""".stripMargin
          case Type.BooleanMatrix =>
            val dim1 = parseGenExpr(d.lengthValue1)
            val dim2 = parseGenExpr(d.lengthValue2)
            s"""|bool ${d.name}[${dim1}];
                |for (int i = 0; i < ${dim1}; i++) {
                |  for (int j = 0; j < ${dim2}; j++) {
                |    ${d.name}[i][j] = false;
                |  }
                |}
                |""".stripMargin
        }
        addLine(generatedLine)

      case Assign(id, name, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$name = $genValue;", id)

      case Call(id, value) =>
        val genValue = parseGenExpr(value)
        addLine(s"$genValue;", id)

      case Input(id, name, promptOpt) =>
        promptOpt
          .orElse {
            Option.when(programAst.config.useInputPrompt)(s"Please enter $name: ")
          }
          .foreach { prompt =>
            addLine(s"""printf("$prompt");""", id)
          }
        val tpe = Try(symTab.getSymbolVar("", name).tpe).toOption.getOrElse(Type.String)
        val (format, pointer) = tpe match
          case Type.Integer | Type.IntegerArray | Type.IntegerMatrix => ("%d", s"&${name}")
          case Type.Real | Type.RealArray | Type.RealMatrix          => ("%d", s"&${name}")
          case Type.Boolean | Type.BooleanArray | Type.BooleanMatrix => ("%d", s"&${name}")
          case Type.String | Type.StringArray | Type.StringMatrix    => ("%d", name) // array is pointer
          case Type.Void                                             => sys.error("Void cannot be entered")
        addLine(s"""scanf("${format}", ${pointer});""", id)

      case Output(id, value, newline) =>
        val genValue = parseGenExpr(value)
        val text = s"printf($genValue);"
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

  import PredefinedFunction.*
  override def predefFun(name: String, genArgs: List[String]): String = {
    def argOpt(idx: Int) = genArgs.lift(idx).getOrElse("")
    PredefinedFunction.withName(name).get match {
      case Abs             => s"abs(${argOpt(0)})"
      case Floor           => s"floor(${argOpt(0)})"
      case Ceil            => s"ceil(${argOpt(0)})"
      case RandomInteger   => s"(rand() % ${argOpt(0)})"
      case Sin             => s"sin(${argOpt(0)})"
      case Cos             => s"cos(${argOpt(0)})"
      case Tan             => s"tan(${argOpt(0)})"
      case Ln              => s"log(${argOpt(0)})"
      case Log10           => s"log10(${argOpt(0)})"
      case Log2            => s"log10(${argOpt(0)})/log10(2)"
      case Sqrt            => s"Math.sqrt(${argOpt(0)})"
      case Pow             => s"Math.pow(${argOpt(0)}, ${argOpt(1)})"
      case Length          => s"${argOpt(0)}.length()"
      case CharAt          => s"${argOpt(0)}.charAt(${argOpt(1)})"
      case RealToInteger   => s"(int)${argOpt(0)}"
      case StringToInteger => s"atoi(${argOpt(0)})"
      case ReadInput       => s"""scanf("%s", ${argOpt(0)})"""
      case ClearOutput     => "printf(\"\\e[1;1H\\e[2J\")"
      case NumRows         => s"sizeof(${argOpt(0)}) / sizeof(${argOpt(0)}[0])"
      case NumCols =>
        val arr = argOpt(0)
        s"(sizeof(${arr})/sizeof(${arr}[0][0])) / (sizeof(${arr})/sizeof(${arr}[0]))"
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
      case IntegerArray  => "int"
      case IntegerMatrix => "int"
      case Real          => "double"
      case RealArray     => "double"
      case RealMatrix    => "double"
      case String        => "char"
      case StringArray   => "char"
      case StringMatrix  => "char"
      case Boolean       => "bool"
      case BooleanArray  => "bool"
      case BooleanMatrix => "bool"

}
