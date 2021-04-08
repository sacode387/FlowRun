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
/* AST, represented visually! */

enum Statement(val id: String) derives NativeConverter:
  case Begin extends Statement("beginId")
  case End extends Statement("endId")
  case Declare(override val id: String, name: String, tpe: Expression.Type, initValue: Option[Expression]) extends Statement(id)
  case Assign(override val id: String, name: String, value: Expression) extends Statement(id)
  case Input(override val id: String, name: String) extends Statement(id)
  case Output(override val id: String, value: Expression) extends Statement(id)
  case Block(override val id: String, statements: List[Statement] = List.empty) extends Statement(id)
  case BlockEnd(override val id: String) extends Statement(id)
  case If(
    override val id: String,
    condition: Expression,
    trueBlock: Block,
    falseBlock: Block
  ) extends Statement(id)

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
