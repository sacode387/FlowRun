package ba.sake.flowrun

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

case class Expression(boolOrComparison: BoolOrComparison, boolOrComparisons: Seq[BoolOrComparison])

object Expression {
  enum Type {
    case Void
    case Integer
    case Real
    case String
    case Boolean
  }
}

case class BoolOrComparison(boolAndComparison: BoolAndComparison, boolAndComparisons: Seq[BoolAndComparison])

case class BoolAndComparison(numComparison: NumComparison, numComparisons: Seq[NumComparisonOpt])

case class NumComparison(term: Term, terms: Seq[TermOpt])
case class NumComparisonOpt(op: Token, numComparison: NumComparison)

case class Term(factor: Factor, factors: Seq[FactorOpt])
case class TermOpt(op: Token, term: Term)

case class Factor(unary: Unary, unaries: Seq[UnaryOpt])
case class FactorOpt(op: Token, factor: Factor)

enum Unary {
  case Prefixed(op: Token, unary: Unary)
  case Simple(atom: Atom)
}
case class UnaryOpt(op: Token, unary: Unary)

enum Atom {
  case NumberLit(value: Double)
  case StringLit(value: String)
  case Identifier(name: String)
  case TrueLit
  case FalseLit
  case NullLit
  case Parens(expression: Expression)
}

///////////////////////////////////////////////
/* AST, represented visually! */

enum Statement(val id: String) {
  case Begin extends Statement("beginId")
  case End extends Statement("endId")
  case Declare(override val id: String, name: String, tpe: Expression.Type, initValue: Option[Expression]) extends Statement(id)
  case Assign(override val id: String, name: String, value: Expression) extends Statement(id)
  case Input(override val id: String, name: String, value: Expression) extends Statement(id)
  case Output(override val id: String, value: Expression) extends Statement(id)
  case Block(override val id: String, statements: Seq[Statement] = Seq.empty) extends Statement(id)
  case BlockEnd(override val id: String) extends Statement(id)
  case If(
    override val id: String,
    condition: Expression,
    trueBlock: Block,
    falseBlock: Block
  ) extends Statement(id)
}

case class Program(
  statements: Seq[Statement] = Seq(
    Statement.Begin, Statement.End
  )
)
