package dev.sacode.flowrun

import scala.util.boundary, boundary.break
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

object DomUtils {

  def isTouchDevice: Boolean =
    dom.window.matchMedia("(pointer: coarse)").matches

  def getNearestSvgNode(event: dom.MouseEvent): (String, dom.svg.G) = boundary {
    
    getSvgNode(event.target).getOrElse {
      for (i <- 1 to 48) { // search around mouse click for nearby edges
        val nearNodes = List(
          dom.document.elementFromPoint(event.clientX + i, event.clientY),
          dom.document.elementFromPoint(event.clientX - i, event.clientY),
          dom.document.elementFromPoint(event.clientX, event.clientY + i),
          dom.document.elementFromPoint(event.clientX, event.clientY - i)
        ).flatMap(getSvgNode)
        val maybeNode = nearNodes.headOption
        if maybeNode.isDefined && maybeNode.get._1 == "EDGE" then break(maybeNode.get)
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
