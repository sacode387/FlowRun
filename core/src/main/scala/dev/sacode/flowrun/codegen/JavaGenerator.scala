package dev.sacode.flowrun.codegen


import scala.util.Try
import dev.sacode.flowrun.toIdentifier
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol

class JavaGenerator(override val programAst: Program) extends CodeGenerator {

  def generate: Try[CodeGenRes] = Try {

    addLine("import java.util.*;", programAst.main.id)
    addEmptyLine()
    addLine(s"public class ${programAst.name.toIdentifier} {", programAst.main.id)

    incrIndent()
    if programAst.hasInputs then
      addLine("static Scanner scanner = new Scanner(System.in);", "")
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
      "public static void main(String args[]) {",
      function.statements.head.id
    )

    incrIndent()
    function.statements.foreach(genStatement)
    decrIndent()

    addLine("}", function.id)

    symTab.exitScope()
  }

  private def genFunction(function: Function): Unit = {
    symTab.enterScope(function.id, function.name)

    val params = function.parameters.map(p => s"${p.name}: ${getType(p.tpe)}").mkString(", ")
    addEmptyLine()
    addLine(
      s"public static ${getType(function.tpe)} ${function.name}($params) {", 
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
        addLine(s"${getType(tpe)} $name = $initValue;", id)
        
      case Assign(id, name, value) =>
        addLine(s"$name = $value;", id)

      case Call(id, value) =>
        addLine(s"$value;", id)

      case Input(id, name, promptOpt) =>

        val prompt = promptOpt.getOrElse(s"Please enter $name: ")
        addLine(s"""System.out.print("$prompt");""", id)

        val symOpt = Try(symTab.getSymbolVar("", name)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        addLine(s"$name = scanner.$readFun();", id)

      case Output(id, value, newline) =>
        val text =
          if newline then s"System.out.println($value);"
          else s"System.out.print($value);"
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
        addLine(s"for (int $varName = $start; i <= $end; i += $incr) {", id)
        genStatement(block)
        addLine("}", id)

  private def getType(tpe: Expression.Type): String =
    import Expression.Type, Type._
    tpe match
      case Void    => "void"
      case Integer => "int"
      case Real    => "double"
      case String  => "String"
      case Boolean => "boolean"

  private def readFunction(tpeOpt: Option[Type]): String = tpeOpt match
    case None => "nextLine"
    case Some(tpe) =>
      tpe match
        case Type.Integer => "nextInt"
        case Type.Real    => "nextDouble"
        case Type.Boolean => "nextBoolean"
        case _            => "nextLine"

}

  

    /*
    val statements = programAst.main.statements.map(genStatement).map(_.indented(indent)).filterNot(_.trim.isEmpty)
    val functions = programAst.functions.map(genFunction)
    val maybeScanner = if initInput then "\nstatic Scanner scanner = new Scanner(System.in);\n\n" else ""
    s"""|import java.util.*;
        |
        |public class ${programAst.name.toIdentifier} {
        |${maybeScanner.indented(indent)}
        |${indent.spaces}public static void main(String args[]) {
        |${statements.mkString("\n")}
        |${indent.spaces}}
        |
        |${functions.mkString("\n\n")}
        |}
        |""".stripMargin.trim*/
  
/*
  private def genFunction(function: Function): String = {
    symTab.enterScope(function.id, function.name)
    val statements = function.statements.map(genStatement).filterNot(_.trim.isEmpty)
    val params = function.parameters.map(p => s"${getType(p.tpe)} ${p.name}").mkString(", ")
    symTab.exitScope()
    s"""|public static ${getType(function.tpe)} ${function.name}($params) {
        |${statements.mkString("\n")}
        |}
        |""".stripMargin.indented(indent)
  }

  private def genStatement(stmt: Statement): String =
    import Statement._
    stmt match
      case _: Begin =>
        ""
      case Declare(id, name, tpe, maybeInitValue) =>
        val key = SymbolKey(name, Symbol.Kind.Variable, id)
        symTab.add(id, key, tpe, None)
        val initValue = maybeInitValue.map(v => s" = $v").getOrElse("")
        s"${getType(tpe)} $name$initValue;".indented(indent)
      case Assign(_, name, value) =>
        s"$name = $value;".indented(indent)
      case Call(_, value) =>
        s"$value;".indented(indent)
      case Input(_, name, prompt) =>
        initInput = true
        val symOpt = Try(symTab.getSymbolVar("", name)).toOption
        val readFun = readFunction(symOpt.map(_.tpe))
        s"$name = scanner.$readFun();".indented(indent)
      case Output(_, value, newline) =>
        s"System.out.println($value);".indented(indent)
      case Block(blockId, statements) =>
        statements.map(genStatement).mkString("\n")
      case Return(_, maybeValue) =>
        maybeValue.map(v => s"return $v;").getOrElse("").indented(indent)
      case If(_, condition, trueBlock, falseBlock) =>
        s"""|if ($condition) {
            |${genStatement(trueBlock)}
            |} else {
            |${genStatement(falseBlock)}
            |}""".stripMargin.trim.indented(indent)
      case While(_, condition, block) =>
        s"""|while ($condition) {
            |${genStatement(block)}
            |}""".stripMargin.trim.indented(indent)
      case DoWhile(_, condition, block) =>
        s"""|do {
            |${genStatement(block)}
            |} while ($condition);""".stripMargin.trim.indented(indent)
      case ForLoop(_, varName, start, incr, end, block) =>
        s"""|for (int $varName = $start; i <= $end; i += $incr) {
            |${genStatement(block)}
            |}""".stripMargin.trim.indented(indent)
*/
  
