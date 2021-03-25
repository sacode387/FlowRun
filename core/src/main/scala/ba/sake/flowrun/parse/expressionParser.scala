package ba.sake.flowrun
package parse

import scala.collection.mutable
import ba.sake.flowrun.parse.Token, Token.Type

/**
  * Expression parser.
  *
  * @param nodeId ID of the node that contains this expression.
  * @param allTokens Tokens of this expression.
  */
final class ExpressionParser(nodeId: String, allTokens: List[Token]) {

  private var tokens = allTokens
  private var lookahead = tokens.head
 
  def parse(): Expression =
    val res = expression()
    if lookahead.tpe != Type.EOF then
      throw ParseException(s"Unexpected symbol '$lookahead' found", nodeId)
    else res
  
  private def expression(): Expression =
    Expression(boolOrComparison(), boolOrComparisons())

  private def boolOrComparison(): BoolOrComparison =
    BoolOrComparison(boolAndComparison(), boolAndComparisons())

  private def boolOrComparisons(): Seq[BoolOrComparison] =
    val opts = mutable.ArrayBuffer.empty[BoolOrComparison]
    while Type.Or == lookahead.tpe do
      eat(lookahead.tpe)
      opts += boolOrComparison()
    opts.toSeq
  
  private def boolAndComparison(): BoolAndComparison =
    BoolAndComparison(numComparison(), numComparisons())

  private def boolAndComparisons(): Seq[BoolAndComparison] =
    val opts = mutable.ArrayBuffer.empty[BoolAndComparison]
    while Type.And == lookahead.tpe do
      eat(lookahead.tpe)
      opts += boolAndComparison()
    opts.toSeq

  private def numComparison(): NumComparison =
    NumComparison(term(), terms())

  private def numComparisons(): Seq[NumComparisonOpt] =
    val opts = mutable.ArrayBuffer.empty[NumComparisonOpt]
    while Set(Type.EqualsEquals, Type.NotEquals).contains(lookahead.tpe) do
      val op = eat(lookahead.tpe)
      opts += NumComparisonOpt(op, numComparison())
    opts.toSeq

  private def term(): Term =
    Term(factor(), factors())
  
  private def terms(): Seq[TermOpt] =
    val opts = mutable.ArrayBuffer.empty[TermOpt]
    while Set(Type.Gt, Type.GtEq, Type.Lt, Type.LtEq).contains(lookahead.tpe) do
      val op = eat(lookahead.tpe)
      opts += TermOpt(op, term())
    opts.toSeq
  
  private def factor(): Factor =
    Factor(unary(), unaries())

  private def factors(): Seq[FactorOpt] =
    val opts = mutable.ArrayBuffer.empty[FactorOpt]
    while Set(Type.Plus, Type.Minus).contains(lookahead.tpe) do
      val op = eat(lookahead.tpe)
      opts += FactorOpt(op, factor())
    opts.toSeq
  
  private def unaries(): Seq[UnaryOpt] =
    val opts = mutable.ArrayBuffer.empty[UnaryOpt]
    while Set(Type.Times, Type.Div, Type.Mod).contains(lookahead.tpe) do
      val op = eat(lookahead.tpe)
      opts += UnaryOpt(op, unary())
    opts.toSeq
    
  private def unary(): Unary =
    lookahead.tpe match
      case Type.Not =>
        val op = eat(Type.Not)
        Unary.Prefixed(op, unary())
      case Type.Minus =>
        val op = eat(Type.Minus)
        Unary.Prefixed(op, unary())
      case _ =>
        Unary.Simple(atom())
  
  private def atom(): Atom =
    import Atom._
    lookahead.tpe match
      case Type.Null =>
        eat(Type.Null)
        NullLit
      case Type.True =>
        eat(Type.True)
        TrueLit
      case Type.False => 
        eat(Type.False)
        FalseLit
      case Type.Integer => 
        val num = eat(Type.Integer)
        NumberLit(num.text.toDouble)
      case Type.Real => 
        val num = eat(Type.Real)
        NumberLit(num.text.toDouble)
      case Type.String =>
        val str = eat(Type.String)
        StringLit(str.text)
      case Type.Identifier =>
        val id = eat(Type.Identifier)
        Identifier(id.text)
      case Type.LeftParen =>
        eat(Type.LeftParen)
        val res = Parens(expression())
        eat(Type.RightParen)
        res
      case _ => error(s"Unexpected token: $lookahead")

  private def eat(tpe: Type): Token =
    val res = lookahead
    if lookahead.tpe != tpe then
      error(s"Expected: $tpe, got: ${lookahead.tpe} at position ${lookahead.pos}")
    tokens = tokens.tail
    lookahead = tokens.head
    res

  private def error(msg: String): Nothing =
    throw ParseException(msg, nodeId)

}

final class ParseException(
  message: String,
  val nodeId: String
) extends RuntimeException(message)