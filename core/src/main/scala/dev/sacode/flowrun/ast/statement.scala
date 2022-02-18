package dev.sacode.flowrun.ast

import org.getshaka.nativeconverter.NativeConverter
import dev.sacode.flowrun.eval.Interpreter.State

///////////////////////////////////////////////
/* AST, of visual statements.
 * We store exprs as String-s,
 * they're small and interpreted anyway.
 */

sealed trait Statement(val id: String) derives NativeConverter:
  def duplicated: Statement = this
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
    override def duplicated: Declare = copy(id = AST.newId)
    def label =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name$maybeExprText"
    override def verboseLabel =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name: $tpe$maybeExprText"

  case class Assign(override val id: String, name: String, value: String) extends Statement(id):
    override def duplicated: Assign = copy(id = AST.newId)
    def label = s"$name = $value"

  case class Call(override val id: String, value: String) extends Statement(id):
    override def duplicated: Call = copy(id = AST.newId)
    def label = value

  case class Input(override val id: String, name: String) extends Statement(id):
    override def duplicated: Input = copy(id = AST.newId)
    def label = name

  case class Output(override val id: String, value: String) extends Statement(id):
    override def duplicated: Output = copy(id = AST.newId)
    def label = value

  case class Block(override val id: String, statements: List[Statement] = List.empty) extends Statement(id):
    override def duplicated: Block = copy(id = AST.newId, statements = statements.map(_.duplicated))
    def label = ""

  case class Return(
      override val id: String,
      maybeValue: Option[String] = None
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId)
    def label = maybeValue.map(e => s"return $e").getOrElse("return")

  case class If(
      override val id: String,
      condition: String,
      trueBlock: Block,
      falseBlock: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, trueBlock = trueBlock.duplicated, falseBlock = falseBlock.duplicated)
    def label = condition.toString

  case class While(
      override val id: String,
      condition: String,
      body: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    def label = condition.toString

  case class DoWhile(
      override val id: String,
      condition: String,
      body: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    def label = condition.toString

  case class ForLoop(
      override val id: String,
      varName: String,
      start: String,
      incr: String,
      end: String,
      body: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    def label = s"$varName = $start to $end by $incr"

  // utils
  def name(stmt: Statement, funName: String): String =
    getName(stmt, funName).getOrElse("")
  def hasName(stmt: Statement, funName: String): Boolean =
    getName(stmt, funName).isDefined

  private def getName(stmt: Statement, funName: String): Option[String] = stmt match
    case _: Begin                     => Some(funName)
    case Declare(_, name, _, _)       => Some(name)
    case Assign(_, name, _)           => Some(name)
    case Input(_, name)               => Some(name)
    case ForLoop(_, name, _, _, _, _) => Some(name)
    case _                            => None

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
    case Declare(_, _, _, expr)        => Some(expr.getOrElse(""))
    case Assign(_, _, expr)            => Some(expr)
    case Output(_, expr)               => Some(expr)
    case Call(_, expr)                 => Some(expr)
    case Return(_, expr)               => Some(expr.getOrElse(""))
    case If(_, expr, _, _)             => Some(expr)
    case While(_, expr, _)             => Some(expr)
    case DoWhile(_, expr, _)           => Some(expr)
    case ForLoop(_, _, start, _, _, _) => Some(start)
    case _                             => None

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
