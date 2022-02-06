package dev.sacode.flowrun

import org.getshaka.nativeconverter.NativeConverter

import dev.sacode.flowrun.parse.Token
import java.util.UUID
import dev.sacode.flowrun.eval.Interpreter.State

/*
EXPRESSION GRAMMAR:

expression          -> boolOrComparison ("||"  boolOrComparison)* ;
boolOrComparison    -> boolAndComparison ("&&"  boolAndComparison)* ;
boolAndComparison   -> numComparison (("!=" | "==" ) numComparison)* ;
numComparison       -> term (("<" | ">" | "<=" | ">=") term)* ;
term                -> factor (("+" | "-") factor)* ;
factor              -> unary (("*" |  "/" | "%") unary)* ;
unary               -> ("!" | "-") unary
                    | atom ;
atom                -> NUMBER | STRING | "true" | "false" | "null"
                    | ID
                    | "(" expression ")" ;
 */

case class Expression(boolOrComparison: BoolOrComparison, boolOrComparisons: List[BoolOrComparison])
    derives NativeConverter

object Expression:
  enum Type derives NativeConverter:
    case Void
    case Integer
    case Real
    case String
    case Boolean
  object Type:
    val VarTypes = Type.values.filterNot(_ == Type.Void)

case class BoolOrComparison(
    boolAndComparison: BoolAndComparison,
    boolAndComparisons: List[BoolAndComparison]
) derives NativeConverter

case class BoolAndComparison(numComparison: NumComparison, numComparisons: List[NumComparisonOpt])
    derives NativeConverter

case class NumComparison(term: Term, terms: Option[TermOpt]) derives NativeConverter
case class NumComparisonOpt(op: Token, numComparison: NumComparison) derives NativeConverter

case class Term(factor: Factor, factors: List[FactorOpt]) derives NativeConverter
case class TermOpt(op: Token, term: Term) derives NativeConverter

case class Factor(unary: Unary, unaries: List[UnaryOpt]) derives NativeConverter
case class FactorOpt(op: Token, factor: Factor) derives NativeConverter

enum Unary derives NativeConverter:
  case Prefixed(op: Token, unary: Unary)
  case Simple(atom: Atom)

case class UnaryOpt(op: Token, unary: Unary) derives NativeConverter

enum Atom derives NativeConverter:
  case IntegerLit(value: Long)
  case RealLit(value: Double)
  case StringLit(value: String)
  case Identifier(name: String)
  case TrueLit
  case FalseLit
  case Parens(expression: Expression)
  case FunctionCall(name: String, arguments: List[Expression])

///////////////////////////////////////////////
/* AST, of visual statements.
 * We store exprs as String-s,
 * they're small and interpreted anyway.
 */

sealed trait Statement(val id: String) derives NativeConverter:
  def label: String
  def verboseLabel: String = label

object Statement:

  case class Begin(override val id: String) extends Statement(id):
    def label = "Begin"

  case class Declare(
      override val id: String,
      name: String,
      tpe: Expression.Type,
      initValue: Option[String]
  ) extends Statement(id):
    def label =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name$maybeExprText"
    override def verboseLabel =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name: $tpe$maybeExprText"
  case class Assign(override val id: String, name: String, value: String) extends Statement(id):
    def label = s"$name = $value"

  case class Call(override val id: String, value: String) extends Statement(id):
    def label = value

  case class Input(override val id: String, name: String) extends Statement(id):
    def label = name

  case class Output(override val id: String, value: String) extends Statement(id):
    def label = value

  case class Block(override val id: String, statements: List[Statement] = List.empty) extends Statement(id):
    def label = ""

  case class Return(
      override val id: String,
      maybeValue: Option[String] = None
  ) extends Statement(id):
    def label = maybeValue.map(e => s"return $e").getOrElse("return")

  case class If(
      override val id: String,
      condition: String,
      trueBlock: Block,
      falseBlock: Block
  ) extends Statement(id):
    def label = condition.toString

  case class While(
      override val id: String,
      condition: String,
      body: Block
  ) extends Statement(id):
    def label = condition.toString

  case class DoWhile(
      override val id: String,
      condition: String,
      body: Block
  ) extends Statement(id):
    def label = condition.toString
  
  case class ForLoop(
      override val id: String,
      varName: String,
      start: Int,
      incr: Int,
      end: Int,
      body: Block
  ) extends Statement(id):
    if incr == 0 then
      throw IllegalArgumentException("Increment can not be zero")
    if start <= end && incr < 0 then
      throw IllegalArgumentException("Increment must be positive when start<end")
    if start >= end && incr > 0 then
      throw IllegalArgumentException("Increment must be negative when start>end")
    def label = s"$varName = $start to $end by $incr"
    val comparator = if incr >= 0 then "<=" else ">="

  // utils
  def name(stmt: Statement, funName: String): String =
    getName(stmt, funName).getOrElse("")
  def hasName(stmt: Statement, funName: String): Boolean =
    getName(stmt, funName).isDefined

  private def getName(stmt: Statement, funName: String): Option[String] = stmt match
    case _: Begin               => Some(funName)
    case Declare(_, name, _, _) => Some(name)
    case Assign(_, name, _)     => Some(name)
    case Input(_, name)         => Some(name)
    case _                      => None

  def tpe(stmt: Statement, funTpe: String): String =
    getTpe(stmt, funTpe).getOrElse("")
  def hasTpe(stmt: Statement, funTpe: String): Boolean =
    getTpe(stmt, funTpe).isDefined

  private def getTpe(stmt: Statement, funTpe: String): Option[String] = stmt match
    case Declare(_, _, tpe, _) => Some(tpe.toString)
    case _: Begin              => Some(funTpe)
    case _                     => None

  def expr(stmt: Statement): String =
    getExpr(stmt).getOrElse("")
  def hasExpr(stmt: Statement): Boolean =
    getExpr(stmt).isDefined

  private def getExpr(stmt: Statement): Option[String] = stmt match
    case Declare(_, _, _, expr) => Some(expr.getOrElse(""))
    case Assign(_, _, expr)     => Some(expr)
    case Output(_, expr)        => Some(expr)
    case Call(_, expr)          => Some(expr)
    case Return(_, expr)        => Some(expr.getOrElse(""))
    case If(_, expr, _, _)      => Some(expr)
    case While(_, expr, _)      => Some(expr)
    case DoWhile(_, expr, _)    => Some(expr)
    case _                      => None

  // umm, this logic is a bit hard to explain
  // best to concentrate on it visually
  // look nested ifs
  def widthLeft(stmt: Statement, depth: Int): Int = stmt match
    case stmt: If =>
      val wlMax = stmt.falseBlock.statements.map(s => widthLeft(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.falseBlock.statements.map(s => widthRight(s, depth + 1)).maxOption.getOrElse(0)
      if depth == 0 then wrMax + 1 else wlMax + wrMax + 1
    case stmt: While   => 1
    case stmt: DoWhile => 0
    case stmt: ForLoop => 1
    case _             => 0

  def widthRight(statement: Statement, depth: Int): Int = statement match
    case stmt: If =>
      val wlMax = stmt.trueBlock.statements.map(s => widthLeft(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.trueBlock.statements.map(s => widthRight(s, depth + 1)).maxOption.getOrElse(0)
      if depth == 0 then wlMax + 1 else wlMax + wrMax + 1
    case stmt: While =>
      val wlMax = stmt.body.statements.map(s => widthLeft(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.body.statements.map(s => widthRight(s, depth + 1)).maxOption.getOrElse(0)
      if depth == 0 then wlMax + 1 else wlMax + wrMax + 1
    case stmt: DoWhile =>
      val wlMax = stmt.body.statements.map(s => widthLeft(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.body.statements.map(s => widthRight(s, depth + 1)).maxOption.getOrElse(0)
      if depth == 0 then wlMax + 1 else wlMax + wrMax + 1
    case stmt: ForLoop =>
      val wlMax = stmt.body.statements.map(s => widthLeft(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.body.statements.map(s => widthRight(s, depth + 1)).maxOption.getOrElse(0)
      if depth == 0 then wlMax + 1 else wlMax + wrMax + 1
    case _ => 0

end Statement

case class Function(
    rawId: String,
    name: String,
    parameters: List[Function.Parameter] = List.empty,
    tpe: Expression.Type = Expression.Type.Void,
    statements: List[Statement] = List.empty
) derives NativeConverter:

  val id = s"fun-$rawId"

  def isMain: Boolean = rawId == "main"

  def label: String =
    val title = if isMain then "begin" else name
    val params = if isMain then "" else s"(${parameters.map(p => s"${p.name}").mkString(", ")})"
    s"$title$params"
  def verboseLabel =
    val title = if isMain then "begin" else name
    val params = if isMain then "" else s"(${parameters.map(p => s"${p.name}: ${p.tpe}").mkString(", ")})"
    s"$title$params: $tpe"

object Function:
  case class Parameter(id: String, name: String, tpe: Expression.Type)

case class Program(
    id: String,
    name: String,
    main: Function,
    functions: List[Function] = List.empty
) derives NativeConverter

object AST:
  def newId: String =
    "id_" + UUID.randomUUID.toString.replaceAll("-", "_")
