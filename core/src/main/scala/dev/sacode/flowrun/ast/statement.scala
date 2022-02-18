package dev.sacode.flowrun.ast

import org.getshaka.nativeconverter.NativeConverter

///////////////////////////////////////////////
/* AST, of visual statements.
 * We store exprs as String-s,
 * they're small and interpreted anyway.
 */

sealed trait Statement(val id: String) derives NativeConverter:
  def duplicated: Statement

  def label: String
  def verboseLabel: String = label

end Statement

object Statement:

  case class Begin(override val id: String) extends Statement(id):
    def duplicated: Statement = this
    override def label = "Begin"
  
  case class Return(
      override val id: String,
      maybeValue: Option[String] = None
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId)
    override def label = maybeValue.map(e => s"return $e").getOrElse("return")

  case class Declare(
      override val id: String,
      name: String,
      tpe: Expression.Type,
      initValue: Option[String]
  ) extends Statement(id):
    override def duplicated: Declare = copy(id = AST.newId)
    override def label =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name$maybeExprText"
    override def verboseLabel =
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name: $tpe$maybeExprText"

  case class Assign(override val id: String, name: String, value: String) extends Statement(id):
    override def duplicated: Assign = copy(id = AST.newId)
    override def label = s"$name = $value"

  case class Call(override val id: String, value: String) extends Statement(id):
    override def duplicated: Call = copy(id = AST.newId)
    override def label = value

  case class Input(override val id: String, name: String) extends Statement(id):
    override def duplicated: Input = copy(id = AST.newId)
    override def label = name

  case class Output(override val id: String, value: String) extends Statement(id):
    override def duplicated: Output = copy(id = AST.newId)
    override def label = value

  case class Block(override val id: String, statements: List[Statement] = List.empty) extends Statement(id):
    override def duplicated: Block = copy(id = AST.newId, statements = statements.map(_.duplicated))
    override def label = ""

  case class If(
      override val id: String,
      condition: String,
      trueBlock: Block,
      falseBlock: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, trueBlock = trueBlock.duplicated, falseBlock = falseBlock.duplicated)
    override def label = condition.toString

  case class While(
      override val id: String,
      condition: String,
      body: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    override def label = condition.toString

  case class DoWhile(
      override val id: String,
      condition: String,
      body: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    override def label = condition.toString

  case class ForLoop(
      override val id: String,
      varName: String,
      start: String,
      incr: String,
      end: String,
      body: Block
  ) extends Statement(id):
    override def duplicated = copy(id = AST.newId, body = body.duplicated)
    override def label = s"$varName = $start to $end by $incr"

end Statement
