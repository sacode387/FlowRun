package ba.sake.flowrun

import org.getshaka.nativeconverter.NativeConverter

import ba.sake.flowrun.parse.Token

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

case class Expression(boolOrComparison: BoolOrComparison, boolOrComparisons: List[BoolOrComparison]) derives NativeConverter

object Expression:
  enum Type derives NativeConverter:
    case Integer
    case Real
    case String
    case Boolean
    //case Void

case class BoolOrComparison(boolAndComparison: BoolAndComparison, boolAndComparisons: List[BoolAndComparison]) derives NativeConverter

case class BoolAndComparison(numComparison: NumComparison, numComparisons: List[NumComparisonOpt]) derives NativeConverter

// TODO Option[TermOpt] ???
// ne može 1>2>3 ...
case class NumComparison(term: Term, terms: List[TermOpt]) derives NativeConverter
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

///////////////////////////////////////////////
/* AST, represented visually! 
 * We just store exprs as String-s,
 * they're small and interpreted anyway.
 */

sealed trait Statement(val id: String) derives NativeConverter:
  // TODO REMOVE ...
  def label: String

object Statement:
  case object Begin extends Statement("beginId"):
    def label = "begin"
  case object End extends Statement("endId"):
    def label = "end"
  case class Dummy(override val id: String) extends Statement(id):
    def label = ""
  case class Declare(override val id: String, name: String, tpe: Expression.Type, initValue: Option[String]) extends Statement(id):
    def label = {
      val maybeExprText = initValue.map(e => s" = $e").getOrElse("")
      s"$name: $tpe$maybeExprText"
    }
  case class Assign(override val id: String, name: String, value: String) extends Statement(id):
    def label = s"$name = $value"
  case class Input(override val id: String, name: String) extends Statement(id):
    def label = name
  case class Output(override val id: String, value: String) extends Statement(id):
    def label = value
  case class Block(override val id: String, statements: List[Statement] = List.empty) extends Statement(id):
    def label = ""
  case class BlockEnd(override val id: String) extends Statement(id):
    def label = ""
  case class If(
    override val id: String,
    condition: String,
    trueBlock: Block,
    falseBlock: Block
  ) extends Statement(id):
    def label = condition.toString
end Statement

case class Function(
  name: String,
  tpe: Option[Expression.Type] = None,
  statements: List[Statement] = List(
    Statement.Begin, Statement.End
  )
) derives NativeConverter

case class Program(
  name: String,
  main: Function,
  functions: List[Function] = List.empty
) derives NativeConverter
