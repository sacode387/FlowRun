package dev.sacode.flowrun

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.window

import dev.sacode.flowrun.eval.SymbolKey

extension (any: Any) {
  def asDyn: js.Dynamic = any.asInstanceOf[js.Dynamic]
}

extension (str: String) {
  def toGraphvizLbl: String =
    str.replace("\"", "\\\"")
  
  def asInteger: Try[Long] =
    if str.trim.forall(_.isDigit) then Try(str.toLong)
    else Failure(new RuntimeException("Invalid integer"))
  
  def asReal: Try[Double] =
    Try(str.toDouble)
}

def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString

def isTouchDevice: Boolean =
  dom.window.matchMedia("(pointer: coarse)").matches

def getSvgNode(et: dom.EventTarget): (String, dom.svg.G) = {
  var node: dom.EventTarget = et
  while (!js.isUndefined(node)) {
    node match {
      case g: dom.svg.G =>
        if g.className.baseVal == "node" then return ("NODE", g)
        else if g.className.baseVal == "edge" then return ("EDGE", g)
        else node = g.parentNode
      case n: dom.Node =>
        node = n.parentNode
      case _ =>
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

object NameUtils:
  def validateIdentifier(identifier: String): Option[String] =
    val ident = identifier.trim
    if ident.isEmpty then Some("Name must not be empty")
    else if ident.length > 30 then Some("Name can be longer than 100 characters")
    else if !ident.head.isLetter then Some("Name must start with a letter")
    else if ident.matches(".*\\s.*") then Some("Name must not contain spaces")
    else if !ident.matches("[a-zA-Z0-9_]+") then
      Some("Name must contain only letters, numbers or underscore.")
    else if SymbolKey.ReservedWords(ident) then Some("Name must not be a reserved word")
    else None
