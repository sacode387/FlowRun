package dev.sacode.flowrun.ast

import ba.sake.tupson.*

///////////////////////////////////////////////
/* AST, of visual statements.
 * We store exprs as String-s,
 * they're small and interpreted anyway.
 */

sealed trait Statement derives JsonRW:

  def id: String
  def duplicated: Statement

  def label: String
  def verboseLabel: String = label

end Statement

object Statement:

  case class Begin(id: String) extends Statement derives JsonRW:
    def duplicated: Statement = this
    override def label = "Begin"

  case class Return(
      id: String,
      maybeValue: Option[String] = None
  ) extends Statement
      derives JsonRW:
    override def duplicated = copy(id = AST.newId)
    override def label = maybeValue.map(e => s"return $e").getOrElse("return")

  case class Declare(
      id: String,
      name: String,
      tpe: Expression.Type,
      initValue: Option[String]
  ) extends Statement
      derives JsonRW:
    override def duplicated: Declare = copy(id = AST.newId)
    override def label =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name$maybeExprText"
    override def verboseLabel =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name: $tpe$maybeExprText"

  case class Assign(id: String, name: String, value: String) extends Statement derives JsonRW:
    override def duplicated: Assign = copy(id = AST.newId)
    override def label = s"$name = $value"

  case class Call(id: String, value: String) extends Statement derives JsonRW:
    override def duplicated: Call = copy(id = AST.newId)
    override def label = value

  case class Input(id: String, name: String, prompt: Option[String]) extends Statement derives JsonRW:
    override def duplicated: Input = copy(id = AST.newId)
    override def label = name

  case class Output(id: String, value: String, newline: Boolean) extends Statement derives JsonRW:
    override def duplicated: Output = copy(id = AST.newId)
    override def label = value

  case class Block(id: String, statements: List[Statement] = List.empty) extends Statement derives JsonRW:
    override def duplicated: Block = copy(id = AST.newId, statements = statements.map(_.duplicated))
    override def label = ""

  case class If(
      id: String,
      condition: String,
      trueBlock: Block,
      falseBlock: Block
  ) extends Statement
      derives JsonRW:
    override def duplicated = copy(id = AST.newId, trueBlock = trueBlock.duplicated, falseBlock = falseBlock.duplicated)
    override def label = condition.toString

  case class While(
      id: String,
      condition: String,
      body: Block
  ) extends Statement
      derives JsonRW:
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    override def label = condition.toString

  case class DoWhile(
      id: String,
      condition: String,
      body: Block
  ) extends Statement
      derives JsonRW:
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    override def label = condition.toString

  case class ForLoop(
      id: String,
      varName: String,
      start: String,
      incr: String,
      end: String,
      body: Block
  ) extends Statement
      derives JsonRW:
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    override def label = s"$varName = $start to $end by $incr"

  case class Comment(id: String, text: String) extends Statement derives JsonRW:
    override def duplicated: Comment = copy(id = AST.newId)
    override def label = text

end Statement
