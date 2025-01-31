package dev.sacode.flowrun.eval

import dev.sacode.flowrun.ast.Expression.Type
import scala.util.Try

// - we preserve runtime type while evaluating
// - can't disambiguate Double 5.0 and Integer 5 at runtime in JS.. :')
// https://gitter.im/scala-js/scala-js?at=621a185c257a35782502f345

enum RunVal(val tpe: Type, val valueOpt: Option[Any]):
  case NoVal extends RunVal(Type.Void, None)
  case IntegerVal(value: Int) extends RunVal(Type.Integer, Some(value))
  case RealVal(value: Double) extends RunVal(Type.Real, Some(value))
  case StringVal(value: String) extends RunVal(Type.String, Some(value))
  case BooleanVal(value: Boolean) extends RunVal(Type.Boolean, Some(value))
  case IntegerArrayVal(values: Array[Integer]) extends RunVal(Type.IntegerArray, Some(values))
  case RealArrayVal(values: Array[Double]) extends RunVal(Type.RealArray, Some(values))
  case StringArrayVal(values: Array[String]) extends RunVal(Type.StringArray, Some(values))
  case BooleanArrayVal(values: Array[Boolean]) extends RunVal(Type.BooleanArray, Some(values))
  case IntegerMatrixVal(values: Array[Array[Integer]]) extends RunVal(Type.IntegerMatrix, Some(values))
  case RealMatrixVal(values: Array[Array[Double]]) extends RunVal(Type.RealMatrix, Some(values))
  case StringMatrixVal(values: Array[Array[String]]) extends RunVal(Type.StringMatrix, Some(values))
  case BooleanMatrixVal(values: Array[Array[Boolean]]) extends RunVal(Type.BooleanMatrix, Some(values))

  def valueString: String = this match
    case NoVal                    => "()"
    case IntegerVal(value)        => s"$value"
    case RealVal(value)           => s"$value"
    case StringVal(value)         => s"$value"
    case BooleanVal(value)        => s"$value"
    case IntegerArrayVal(values)  => values.mkString("[", ",", "]")
    case RealArrayVal(values)     => values.mkString("[", ",", "]")
    case StringArrayVal(values)   => values.mkString("[", ",", "]")
    case BooleanArrayVal(values)  => values.mkString("[", ",", "]")
    case IntegerMatrixVal(values) => values.map(_.mkString("[", ",", "]")).mkString("[", ",\n ", "]")
    case RealMatrixVal(values)    => values.map(_.mkString("[", ",", "]")).mkString("[", ",\n ", "]")
    case StringMatrixVal(values)  => values.map(_.mkString("[", ",", "]")).mkString("[", ",\n ", "]")
    case BooleanMatrixVal(values) => values.map(_.mkString("[", ",", "]")).mkString("[", ",\n ", "]")

  def valueAndTypeString: String = this match
    case NoVal             => "(): Void"
    case IntegerVal(value) => s"$value: Integer"
    case RealVal(value)    => s"$value: Real"
    case StringVal(value)  => s"$value: String"
    case BooleanVal(value) => s"$value: Boolean"
    case _: IntegerArrayVal =>
      s"$valueString: Integer[]"
    case _: RealArrayVal =>
      s"$valueString: Real[]"
    case _: StringArrayVal =>
      s"$valueString: String[]"
    case _: BooleanArrayVal =>
      s"$valueString: Boolean[]"
    case _: IntegerMatrixVal =>
      s"$valueString: Integer[][]"
    case _: RealMatrixVal =>
      s"$valueString: Real[][]"
    case _: StringMatrixVal =>
      s"$valueString: String[][]"
    case _: BooleanMatrixVal =>
      s"$valueString: Boolean[][]"

  override def toString: String = valueAndTypeString

  // adapt an integer to real
  def promote(nodeId: String, expectedName: String, expectedTpe: Type): RunVal =
    if expectedTpe == Type.Real then
      this match
        case rv: RealVal    => rv
        case iv: IntegerVal => RealVal(iv.value.toDouble)
        case otherVal =>
          throw EvalException(
            s"Expected '$expectedName: ${expectedTpe.pretty}' but got '${valueOpt.get}: ${tpe.pretty}'",
            nodeId
          )
    else if tpe != expectedTpe then
      throw EvalException(
        s"Expected '$expectedName: ${expectedTpe.pretty}' but got '${valueOpt.get}: ${tpe.pretty}'",
        nodeId
      )
    else this

object RunVal:
  import Type.*

  def fromString(inputValue: String): RunVal = Try(BooleanVal(inputValue.toBoolean))
    .orElse(
      Try(IntegerVal(inputValue.toInt)).orElse(
        Try(RealVal(inputValue.toDouble))
      )
    )
    .getOrElse(StringVal(inputValue))

  extension (iv: IntegerVal) def transform(f: Int => Int): IntegerVal = IntegerVal(f(iv.value))

  extension (iv: RealVal) def transform(f: Double => Double): RealVal = RealVal(f(iv.value))

  extension (iv: StringVal) def transform(f: String => String): StringVal = StringVal(f(iv.value))

  extension (iv: BooleanVal) def transform(f: Boolean => Boolean): BooleanVal = BooleanVal(f(iv.value))
