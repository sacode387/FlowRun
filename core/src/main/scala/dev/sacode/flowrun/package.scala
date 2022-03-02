package dev.sacode.flowrun

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.window
import scalatags.JsDom.all._

import dev.sacode.flowrun.eval.SymbolKey
import dev.sacode.flowrun.eval.RunVal
import dev.sacode.flowrun.ast.*
import java.time.format.DateTimeFormatter
import java.time.ZoneId
import java.time.ZoneOffset

val samp = tag("samp")

val DTF = DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss").withZone(ZoneId.from(ZoneOffset.UTC))

extension (any: Any) {
  def asDyn: js.Dynamic = any.asInstanceOf[js.Dynamic]
}

extension (str: String) {

  def toGraphvizLbl: String =
    str.replace("\"", "\\\"")

  def toIdentifier: String =
    str.replaceAll("\\s", "")

  def indented(x: Int): String =
    val spaces = " " * x
    str.linesIterator
      .map { line =>
        spaces + line
      }
      .mkString("\n")
}

extension (integer: Int) {
  def spaces: String =
    " " * integer
}

extension (stmt: Statement) {
  def firstNodeId: String = stmt match {
    case _: Statement.DoWhile => s"end_${stmt.id}"
    case _                    => stmt.id
  }
}

object TypeUtils:
  import Expression.Type
  import RunVal.*

  // check if assignable, and optionally casts the type
  def getValue(nodeId: String, expectedType: Type, value: Any): Try[RunVal] = Try {
     IntegerVal(1)
    /*// prevent small numbers to be shown as Byte,Short etc
    val valueType = value.getClass.getSimpleName match
      case "Byte" | "Short" | "Integer" | "Long" => "Integer"
      case "Float" | "Double"                    => "Real"
      case "Boolean"                             => "Boolean"
      case _                                     => value.getClass.getSimpleName
    
    (expectedType, valueType) match {
      case (Type.Integer, "Integer")       => IntegerVal(value)
      case (Type.Real, "Real" | "Integer") => RealVal(value)
      case (Type.String, "String")         => StringVal(value)
      case (Type.Boolean, "Boolean")       => BooleanVal(value)
      case (expectedType, _) =>
        val valueStr = if valueType == "String" then s"\"$value\"" else value
        throw eval.EvalException(
          s"Expected '$expectedType' but got '$valueType' for value $valueStr ",
          nodeId
        )
    }*/
  }

object NameUtils:
  private val IdentMaxChars = 30
  def validateIdentifier(identifier: String): Option[String] =
    val ident = identifier.trim
    if ident.isEmpty then Some("Name must not be empty")
    else if ident.length > IdentMaxChars then Some(s"Name can be longer than $IdentMaxChars characters")
    else if !ident.head.isLetter then Some("Name must start with a letter")
    else if ident.matches(".*\\s.*") then Some("Name must not contain spaces")
    else if !ident.matches("[a-zA-Z0-9_]+") then Some("Name must contain only letters, numbers or underscore.")
    else if SymbolKey.ReservedWords(ident) then Some("Name must not be a reserved word")
    else None

object DomUtils {

  def isTouchDevice: Boolean =
    dom.window.matchMedia("(pointer: coarse)").matches

  def getNearestSvgNode(event: dom.MouseEvent): (String, dom.svg.G) = {
    getSvgNode(event.target).getOrElse {
      for (i <- 1 to 15) { // search around mouse click for nearby edges
        val nearNodes = List(
          dom.document.elementFromPoint(event.clientX + i, event.clientY),
          dom.document.elementFromPoint(event.clientX - i, event.clientY),
          dom.document.elementFromPoint(event.clientX, event.clientY + i),
          dom.document.elementFromPoint(event.clientX, event.clientY - i)
        ).flatMap(getSvgNode)
        val maybeNode = nearNodes.headOption
        if maybeNode.isDefined && maybeNode.get._1 == "EDGE" then return maybeNode.get
      }
      ("", null)
    }
  }

  private def getSvgNode(et: dom.EventTarget): Option[(String, dom.svg.G)] = {
    var node: dom.EventTarget = et
    while (!js.isUndefined(node)) {
      node match {
        case g: dom.svg.G =>
          if g.className.baseVal.contains("node") then return Some(("NODE", g))
          else if g.className.baseVal.contains("edge") then return Some(("EDGE", g))
          else node = g.parentNode
        case n: dom.Node =>
          node = n.parentNode
        case _ =>
          return None
      }
    }
    None
  }
}
