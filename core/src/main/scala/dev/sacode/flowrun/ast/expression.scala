package dev.sacode.flowrun.ast

import ba.sake.tupson.*
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

object Expression:
  enum Type derives JsonRW:
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
)

case class BoolAndComparison(numComparison: NumComparison, numComparisons: List[NumComparisonOpt])

case class NumComparison(term: Term, terms: Option[TermOpt])
case class NumComparisonOpt(op: Token, numComparison: NumComparison)

case class Term(factor: Factor, factors: List[FactorOpt])
case class TermOpt(op: Token, term: Term)

case class Factor(unary: Unary, unaries: List[UnaryOpt])
case class FactorOpt(op: Token, factor: Factor)

enum Unary:
  case Prefixed(op: Token, unary: Unary)
  case Simple(atom: Atom)

case class UnaryOpt(op: Token, unary: Unary)

enum Atom:
  case IntegerLit(value: Int)
  case RealLit(value: Double)
  case StringLit(value: String)
  case Identifier(name: String)
  case TrueLit
  case FalseLit
  case Parens(expression: Expression)
  case FunctionCall(name: String, arguments: List[Expression])
