package dev.sacode.flowrun

import scala.util.Try
import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.window

extension (any: Any) {
  def asDyn: js.Dynamic = any.asInstanceOf[js.Dynamic]
}

extension (str: String) {
  def toGraphvizLbl: String =
    str.replace("\"", "\\\"")
}

def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString

def isTouchDevice: Boolean =
  dom.window.matchMedia("(pointer: coarse)").matches

def getSvgNode(et: dom.EventTarget): (String, dom.svg.G) = {
    var node: dom.EventTarget = et
    while (!js.isUndefined(node)) {
      println(node)
      node match {
        case g: dom.svg.G =>
          if g.className.baseVal == "node" then
            return ("NODE", g)
          else if g.className.baseVal == "edge" then
            return ("EDGE", g)
          else
            node = g.parentNode
        case n: dom.Node =>
          node = n.parentNode
        case _ =>
          println("hmmmmmm")
          return ("", null)
      }
    }
    ("", null)
  }

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
        val valueStr = if valueType == "String" then s"\"$value\"" else value
        throw eval.EvalException(
          s"Expected type '$expectedType' but got type '$valueType' for value $valueStr ",
          nodeId
        )
    }
  }
