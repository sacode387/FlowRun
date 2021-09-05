package dev.sacode.flowrun

import org.getshaka.nativeconverter.NativeConverter

import dev.sacode.flowrun.parse.Token

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
  case NumberLit(value: Double)
  case StringLit(value: String)
  case Identifier(name: String)
  case TrueLit
  case FalseLit
  case Parens(expression: Expression)
  case FunctionCall(name: String, arguments: List[Expression]) extends Atom

///////////////////////////////////////////////
/* AST, represented visually!
 * We just store exprs as String-s,
 * they're small and interpreted anyway.
 */

sealed trait Statement(val id: String) derives NativeConverter:
  def label: String

object Statement:
  case class Dummy(override val id: String) extends Statement(id):
    def label = ""
  case class Declare(
      override val id: String,
      name: String,
      tpe: Expression.Type,
      initValue: Option[String]
  ) extends Statement(id):
    def label =
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
  case class Block(override val id: String, statements: List[Statement] = List.empty)
      extends Statement(id):
    def label = ""

  case class Return(
      override val id: String,
      maybeValue: Option[String] = None
  ) extends Statement(id):
    def label =
      val maybeExprText = maybeValue.map(e => s" $e").getOrElse("")
      s"return$maybeExprText"
  case class If(
      override val id: String,
      condition: String,
      trueBlock: Block,
      falseBlock: Block
  ) extends Statement(id):
    def label = condition.toString
end Statement

case class Function(
    rawId: String,
    name: String,
    parameters: List[(String, Expression.Type)] = List.empty[(String, Expression.Type)],
    tpe: Expression.Type = Expression.Type.Void,
    statements: List[Statement] =
      List.empty[Statement] // java.lang.AssertionError: cannot merge Constraint(...
) derives NativeConverter:

  val id = s"fun-$rawId"

  def isMain: Boolean = rawId == "main"

  def label: String =
    val title = if isMain then "begin" else name
    val params = if isMain then "" else s"(${parameters.map((n, t) => s"$n: $t").mkString(",")})"
    s"$title$params: $tpe"

case class Program(
    id: String,
    name: String,
    main: Function,
    functions: List[Function] = List.empty
) derives NativeConverter