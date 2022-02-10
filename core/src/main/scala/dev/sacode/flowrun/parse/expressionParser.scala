package dev.sacode.flowrun
package parse

import scala.collection.mutable
import dev.sacode.flowrun.parse.Token, Token.Type

/** Expression parser.
  *
  * @param nodeId
  *   ID of the node that contains this expression.
  * @param allTokens
  *   Tokens of this expression.
  */
final class ExpressionParser(nodeId: String, allTokens: List[Token]) {

  private var tokens = allTokens
  private var lookahead = tokens.head

  def parse(): Expression =
    val res = expression()
    if lookahead.tpe != Type.EOF then
      error(s"Expected end of input but found '${lookahead.text}' at index ${lookahead.pos}")
    else res

  private def expression(): Expression =
    Expression(boolOrComparison(), boolOrComparisons())

  private def boolOrComparison(): BoolOrComparison =
    BoolOrComparison(boolAndComparison(), boolAndComparisons())

  private def boolOrComparisons(): List[BoolOrComparison] =
    val opts = mutable.ArrayBuffer.empty[BoolOrComparison]
    while Type.Or == lookahead.tpe do
      eat(lookahead.tpe)
      opts += boolOrComparison()
    opts.toList

  private def boolAndComparison(): BoolAndComparison =
    BoolAndComparison(numComparison(), numComparisons())

  private def boolAndComparisons(): List[BoolAndComparison] =
    val opts = mutable.ArrayBuffer.empty[BoolAndComparison]
    while Type.And == lookahead.tpe do
      eat(lookahead.tpe)
      opts += boolAndComparison()
    opts.toList

  private def numComparison(): NumComparison =
    NumComparison(term(), terms())

  private def numComparisons(): List[NumComparisonOpt] =
    val opts = mutable.ArrayBuffer.empty[NumComparisonOpt]
    while Set(Type.EqualsEquals, Type.NotEquals).contains(lookahead.tpe) do
      val op = eat(lookahead.tpe)
      opts += NumComparisonOpt(op, numComparison())
    opts.toList

  private def term(): Term =
    Term(factor(), factors())

  private def terms(): Option[TermOpt] =
    if Set(Type.Gt, Type.GtEq, Type.Lt, Type.LtEq).contains(lookahead.tpe) then
      val op = eat(lookahead.tpe)
      Some(TermOpt(op, term()))
    else None

  private def factor(): Factor =
    Factor(unary(), unaries())

  private def factors(): List[FactorOpt] =
    val opts = mutable.ArrayBuffer.empty[FactorOpt]
    while Set(Type.Plus, Type.Minus).contains(lookahead.tpe) do
      val op = eat(lookahead.tpe)
      opts += FactorOpt(op, factor())
    opts.toList

  private def unaries(): List[UnaryOpt] =
    val opts = mutable.ArrayBuffer.empty[UnaryOpt]
    while Set(Type.Times, Type.Div, Type.Mod).contains(lookahead.tpe) do
      val op = eat(lookahead.tpe)
      opts += UnaryOpt(op, unary())
    opts.toList

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
    import Atom.*
    lookahead.tpe match
      case Type.True =>
        eat(Type.True)
        TrueLit
      case Type.False =>
        eat(Type.False)
        FalseLit
      case Type.Integer =>
        val num = eat(Type.Integer)
        val bigintValue = BigInt(num.text)
        if !bigintValue.isValidInt then 
          error("Integer out of range[-2147483648,2147483647]: " + num.text)
        IntegerLit(bigintValue.toInt)
      case Type.Real =>
        val num = eat(Type.Real)
        RealLit(num.text.toDouble)
      case Type.String =>
        val str = eat(Type.String)
        StringLit(str.text)
      case Type.Identifier =>
        val id = eat(Type.Identifier)
        // maybe a function call
        if lookahead.tpe == Type.LeftParen then
          eat(Type.LeftParen)
          if lookahead.tpe == Type.RightParen then
            eat(Type.RightParen)
            FunctionCall(id.text, List.empty)
          else
            val arguments = mutable.ListBuffer(expression())
            while lookahead.tpe == Type.Comma do
              eat(Type.Comma)
              arguments += expression()
            eat(Type.RightParen)
            FunctionCall(id.text, arguments.toList)
        else Identifier(id.text)
      case Type.LeftParen =>
        eat(Type.LeftParen)
        val res = Parens(expression())
        eat(Type.RightParen)
        res
      case _ =>
        if lookahead.tpe == Type.EOF then error(s"Unexpected end of input at index ${lookahead.pos}")
        else error(s"Unexpected symbol '${lookahead.text}' found at index ${lookahead.pos}")

  private def eat(tpe: Type): Token =
    val res = lookahead
    if lookahead.tpe != tpe then error(s"Expected: $tpe, got: ${lookahead.tpe} at position ${lookahead.pos}")
    tokens = tokens.tail
    lookahead = tokens.head
    res

  private def error(msg: String) =
    throw ParseException(msg, nodeId)

}

final class ParseException(
    message: String,
    val nodeId: String
) extends RuntimeException(message)
