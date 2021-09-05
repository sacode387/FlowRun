package dev.sacode.flowrun

import scala.util.Try
import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.window

extension (any: Any) {
  def asDyn: js.Dynamic = any.asInstanceOf[js.Dynamic]
}

def isTouchDevice: Boolean =
  dom.window.matchMedia("(pointer: coarse)").matches

object TypeUtils:
  import Expression.Type
  // check if assignable, and optionally casts the type
  def getUpdateValue(nodeId: String, name: String, expectedType: Type, value: Any): Try[Any] = Try {
    // prevent small numbers to be shown as Byte,Short etc
    val valueType = value.getClass.getSimpleName match
      case "Byte" | "Short" | "Integer" | "Long" => "Integer"
      case "Float" | "Double"                    => "Real"
      case "Boolean"                             => "Boolean"
      case _                                     => value.getClass.getSimpleName
    (expectedType, valueType) match {
      case (Type.Integer, "Integer")       => value
      case (Type.Real, "Real" | "Integer") => value
      case (Type.String, "String")         => value
      case (Type.Boolean, "Boolean")       => value
      case (expectedType, _) =>
        throw eval.EvalException(
          s"Expected type: '$expectedType', got type: '$valueType' for value ${value} ",
          nodeId
        )
    }
  }
