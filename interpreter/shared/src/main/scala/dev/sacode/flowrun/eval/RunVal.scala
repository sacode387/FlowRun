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
  case IntegerArrayVal(values: Seq[Integer]) extends RunVal(Type.IntegerArray, Some(values))
  case RealArrayVal(values: Seq[Double]) extends RunVal(Type.RealArray, Some(values))
  case StringArrayVal(values: Seq[String]) extends RunVal(Type.StringArray, Some(values))
  case BooleanArrayVal(values: Seq[Boolean]) extends RunVal(Type.BooleanArray, Some(values))

  def valueString: String = this match
    case NoVal                   => "()"
    case IntegerVal(value)       => s"$value"
    case RealVal(value)          => s"$value"
    case StringVal(value)        => s"$value"
    case BooleanVal(value)       => s"$value"
    case IntegerArrayVal(values) => values.mkString("[", ",", "]")
    case RealArrayVal(values)    => values.mkString("[", ",", "]")
    case StringArrayVal(values)  => values.mkString("[", ",", "]")
    case BooleanArrayVal(values) => values.mkString("[", ",", "]")

  def valueAndTypeString: String = this match
    case NoVal             => "(): Void"
    case IntegerVal(value) => s"$value: Integer"
    case RealVal(value)    => s"$value: Real"
    case StringVal(value)  => s"$value: String"
    case BooleanVal(value) => s"$value: Boolean"
    case IntegerArrayVal(values) =>
      val arr = values.mkString("[", ",", "]")
      s"$arr: Integer[]"
    case RealArrayVal(values) =>
      val arr = values.mkString("[", ",", "]")
      s"$arr: Real[]"
    case StringArrayVal(values) =>
      val arr = values.mkString("[", ",", "]")
      s"$arr: String[]"
    case BooleanArrayVal(values) =>
      val arr = values.mkString("[", ",", "]")
      s"$arr: Boolean[]"

  override def toString(): String = valueAndTypeString

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
  /*def fromValue(id: String, tpe: Type, value: Any): RunVal = (tpe, value) match
    case (Integer, v: Int)     => IntegerVal(v)
    case (Real, v: Double)     => RealVal(v)
    case (String, v: String)   => StringVal(v)
    case (Boolean, v: Boolean) => BooleanVal(v)
    case _                     => throw EvalException(s"Cant $tpe.", id)*/

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
