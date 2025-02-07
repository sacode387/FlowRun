package dev.sacode.flowrun.codegen

import scala.util.Try

import scala.collection.mutable
import scala.util.Try
import reactify.*
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.*, Expression.Type
import dev.sacode.flowrun.indented
import dev.sacode.flowrun.eval.SymbolTable
import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.Symbol
import dev.sacode.flowrun.ast.*, Expression.Type, Atom.*, Unary.*
import dev.sacode.flowrun.parse.{parseExpr, Token}
import scala.collection.mutable.ListBuffer

// TODO handle reserved keywords
trait CodeGenerator {

  def programAst: Program

  def generate: Try[CodeGenRes]

  def genToken(token: Token): String = token.text

  protected def predefFun(name: String, genArgs: List[String]): String
  protected def funCall(name: String, genArgs: List[String]): String
  protected def identPrefix: String = ""

  private val indentAmount = 2
  private var indent = 0

  private val dummyChannel = Channel[FlowRun.Event]
  protected val symTab = SymbolTable(dummyChannel) // needed for types of vars

  private var lineNum = 1
  protected var lines: ListBuffer[String] = ListBuffer.empty[String]
  protected var stmtLineNums: mutable.Map[String, List[Int]] = mutable.Map.empty.withDefaultValue(List.empty)

  protected def addLine(text: String): Unit =
    addLine(text, "")
  protected def addLine(text: String, stmtId: String): Unit =
    lines += text.indented(indent)
    if stmtId.trim.nonEmpty then
      val lineNums = stmtLineNums.getOrElse(stmtId, List.empty)
      stmtLineNums.put(stmtId, lineNums.appended(lineNum))
    lineNum += 1

  protected def addEmptyLine(): Unit =
    lines += ""
    lineNum += 1

  protected def incrIndent(): Unit =
    indent += indentAmount
  protected def decrIndent(): Unit =
    indent -= indentAmount

  protected def defaultValue(tpe: Type): String = tpe match {
    case Type.Void          => ""
    case Type.Boolean       => "false"
    case Type.Integer       => "0"
    case Type.Real          => "0.0"
    case Type.String        => """ "" """.trim
    case Type.IntegerArray  => sys.error(s"No default value for array or matrix")
  }

  /** Parse FlowRun expression and generate real code
    * @param exprString
    *   expression string to parse
    * @return
    *   generated real code
    */
  protected def parseGenExpr(exprString: String): String =
    genExpr(parseExpr("", exprString))

  private def genExpr(expr: Expression): String = {
    val op = Token(Token.Type.Or, "||", 0)
    val tokenText = genToken(op)
    val bocs = List(genBoolOrComparison(expr.boolOrComparison)) ++ expr.boolOrComparisons.map(boc =>
      s""" $tokenText ${genBoolOrComparison(boc)} """.trim
    )
    bocs.mkString(" ")
  }
  private def genBoolOrComparison(boc: BoolOrComparison): String = {
    val op = Token(Token.Type.And, "&&", 0)
    val tokenText = genToken(op)
    val bacs = List(genBoolAndComparison(boc.boolAndComparison)) ++ boc.boolAndComparisons.map(bac =>
      s""" $tokenText ${genBoolAndComparison(bac)} """.trim
    )
    bacs.mkString(" ")
  }
  private def genBoolAndComparison(bac: BoolAndComparison): String = {
    val ncs = List(genNumComparison(bac.numComparison)) ++ bac.numComparisons.map(nc =>
      s""" ${nc.op.text} ${genNumComparison(nc.numComparison)} """.trim
    )
    ncs.mkString(" ")
  }
  private def genNumComparison(nc: NumComparison): String = {
    val terms = List(genTerm(nc.term)) ++ nc.terms.map(to => s""" ${to.op.text} ${genTerm(to.term)} """.trim)
    terms.mkString(" ")
  }
  private def genTerm(term: Term): String = {
    // TODO handle string concatenation, lang specific...
    val factors =
      List(genFactor(term.factor)) ++ term.factors.map(fo => s""" ${fo.op.text} ${genFactor(fo.factor)} """.trim)
    factors.mkString(" ")
  }
  private def genFactor(factor: Factor): String = {
    val unaries =
      List(genUnary(factor.unary)) ++ factor.unaries.map(uo => s""" ${uo.op.text} ${genUnary(uo.unary)} """.trim)
    unaries.mkString(" ")
  }
  private def genUnary(unary: Unary): String = unary match {
    case Prefixed(op, u) =>
      val tokenText = genToken(op)
      if tokenText.forall(_.isLetter) then s""" $tokenText ${genUnary(u)} """.trim
      else s""" $tokenText${genUnary(u)} """.trim
    case Simple(atom) => genAtom(atom)
  }
  private def genAtom(atom: Atom): String = atom match {
    case IntegerLit(value)  => value.toString
    case RealLit(value)     => value.toString
    case StringLit(value)   => s""" "$value" """.trim
    case Identifier(name)   => identPrefix + name
    case TrueLit            => genToken(Token(Token.Type.True, "true", 0))
    case FalseLit           => genToken(Token(Token.Type.False, "false", 0))
    case Parens(expression) => s""" (${genExpr(expression)}) """.trim
    case FunctionCall(name, arguments) =>
      val genArgs = arguments.map(genExpr)
      PredefinedFunction.withName(name) match
        case Some(f) =>
          predefFun(name, genArgs)
        case None =>
          funCall(name, genArgs)
    case ArrayIndexAccess(name, idxExpr) =>
      val idx = genExpr(idxExpr)
      s""" ${name}[${idx}] """
    case MatrixIndexAccess(name, idxExpr1, idxExpr2) =>
      val idx1 = genExpr(idxExpr1)
      val idx2 = genExpr(idxExpr2)
      s""" ${name}[${idx1}][${idx2}] """
  }

}

case class CodeGenRes(
    lines: List[String],
    stmtLineNums: Map[String, List[Int]]
)
