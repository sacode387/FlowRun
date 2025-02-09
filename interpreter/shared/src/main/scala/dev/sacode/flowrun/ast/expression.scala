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
                    | ID | ID[expression]
                    | "(" expression ")" ;
 */

case class Expression(boolOrComparison: BoolOrComparison, boolOrComparisons: List[BoolOrComparison]) {
  def collectAtoms: List[Atom] =
    (List(boolOrComparison) ++ boolOrComparisons).flatMap(_.collectAtoms)
}

object Expression:
  enum Type derives JsonRW:
    case Void
    case Integer
    case Real
    case String
    case Boolean
    case IntegerArray
    case RealArray
    case StringArray
    case BooleanArray
    case IntegerMatrix
    case RealMatrix
    case StringMatrix
    case BooleanMatrix

    def pretty: String = this match
      case Void          => "Void"
      case Integer       => "Integer"
      case Real          => "Real"
      case String        => "String"
      case Boolean       => "Boolean"
      case IntegerArray  => "Integer[]"
      case RealArray     => "Real[]"
      case StringArray   => "String[]"
      case BooleanArray  => "Boolean[]"
      case IntegerMatrix => "Integer[][]"
      case RealMatrix    => "Real[][]"
      case StringMatrix  => "String[][]"
      case BooleanMatrix => "Boolean[][]"

    def isArray: Boolean = this match
      case IntegerArray => true
      case RealArray    => true
      case StringArray  => true
      case BooleanArray => true
      case _            => false

    def isMatrix: Boolean = this match
      case IntegerMatrix => true
      case RealMatrix    => true
      case StringMatrix  => true
      case BooleanMatrix => true
      case _             => false

  object Type:
    val VarTypes = Type.values.filterNot(_ == Type.Void)

case class BoolOrComparison(
    boolAndComparison: BoolAndComparison,
    boolAndComparisons: List[BoolAndComparison]
) {
  def collectAtoms: List[Atom] =
    (List(boolAndComparison) ++ boolAndComparisons).flatMap(_.collectAtoms)
}

case class BoolAndComparison(numComparison: NumComparison, numComparisons: List[NumComparisonOpt]) {
  def collectAtoms: List[Atom] =
    numComparison.collectAtoms ++ numComparisons.flatMap(_.collectAtoms)
}

case class NumComparison(term: Term, terms: Option[TermOpt]) {
  def collectAtoms: List[Atom] =
    term.collectAtoms ++ terms.toList.flatMap(_.collectAtoms)
}
case class NumComparisonOpt(op: Token, numComparison: NumComparison) {
  def collectAtoms: List[Atom] = numComparison.collectAtoms
}

case class Term(factor: Factor, factors: List[FactorOpt]) {
  def collectAtoms: List[Atom] =
    factor.collectAtoms ++ factors.flatMap(_.collectAtoms)
}
case class TermOpt(op: Token, term: Term) {
  def collectAtoms: List[Atom] = term.collectAtoms
}

case class Factor(unary: Unary, unaries: List[UnaryOpt]) {
  def collectAtoms: List[Atom] =
    unary.collectAtoms ++ unaries.flatMap(_.collectAtoms)
}
case class FactorOpt(op: Token, factor: Factor) {
  def collectAtoms: List[Atom] = factor.collectAtoms
}

enum Unary:
  case Prefixed(op: Token, unary: Unary)
  case Simple(atom: Atom)

  def collectAtoms: List[Atom] = this match
    case Unary.Prefixed(op, unary) => unary.collectAtoms
    case Unary.Simple(atom)        => List(atom)

case class UnaryOpt(op: Token, unary: Unary) {
  def collectAtoms: List[Atom] = unary.collectAtoms
}

enum Atom:
  case IntegerLit(value: Int)
  case RealLit(value: Double)
  case StringLit(value: String)
  case Identifier(name: String)
  case ArrayIndexAccess(name: String, indexExpr: Expression)
  case MatrixIndexAccess(name: String, indexExpr1: Expression, indexExpr2: Expression)
  case TrueLit
  case FalseLit
  case Parens(expression: Expression)
  case FunctionCall(name: String, arguments: List[Expression])
