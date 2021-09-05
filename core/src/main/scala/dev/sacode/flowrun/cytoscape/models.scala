package dev.sacode.flowrun.cytoscape

import java.util.UUID
import scalajs.js
import js.annotation.*
import org.scalajs.dom

@js.native
@JSGlobal
class cytoscape(props: js.Object) extends js.Object {
  def contextMenus(props: js.Object): Unit = js.native
  def add(item: js.Object): js.Dynamic = js.native
  def remove(selector: String): js.Object = js.native
}

/* NODEs */

case class Node(
    label: String,
    tpe: String,
    startId: String = "", // if-metadata
    endId: String = "", // if-metadata
    rawExpr: String = "",
    rawName: String = "",
    rawTpe: String = "",
    rawParams: String = "",
    id: String = UUID.randomUUID.toString
) {
  import scalajs.js.JSConverters.*

  private val (ww: Int, hh: Int) =
    if (Set(Node.Begin, Node.End).contains(tpe)) (70, 30)
    else if (tpe == Node.If) (55, 30)
    else if (tpe == Node.Dummy) (20, 20)
    else if (tpe == Node.IfEnd) (10, 10)
    else if (tpe == Node.Start) (35 max label.length * 11, 30)
    else if (tpe == Node.Return) (35 max label.length * 11, 30)
    else (35 max label.length * 11, 25)

  private val maybeEditable = Option.when(
    Set(
      Node.Start,
      Node.Return,
      Node.Output,
      Node.Input,
      Node.If,
      Node.Declare,
      Node.Assign,
      Node.Call
    ).contains(tpe)
  )(" " + Node.Editable)

  private val maybeRemovable = Option.when(
    Set(Node.Output, Node.Input, Node.If, Node.Declare, Node.Assign, Node.Call).contains(tpe)
  )(" " + Node.Removable)

  def toLit: js.Object = js.Dynamic.literal(
    group = "nodes",
    data = js.Dynamic.literal(
      id = id,
      label = label,
      tpe = tpe,
      width = ww,
      height = hh,
      startId = startId,
      endId = endId,
      rawExpr = rawExpr,
      rawName = rawName,
      rawTpe = rawTpe,
      rawParams = rawParams
    ),
    classes = tpe + maybeEditable.getOrElse("") + maybeRemovable.getOrElse("")
  )
}

object Node {
  val Begin = "Begin"
  val End = "End"
  val Start = "Function"
  val Return = "Return"
  val Output = "Output"
  val Input = "Input"
  val If = "If"
  val IfEnd = "IfEnd"
  val Dummy = "Dummy" // empty node used just for layout dirty-fix
  val Declare = "Declare"
  val Assign = "Assign"
  val Call = "Call"

  val Editable = "Editable"
  val Removable = "Removable"
}

/* EDGEs */
case class Edge(
    source: String,
    target: String,
    label: String = "",
    dir: String = "hor",
    blockId: String = "", // all edges in SAME BRANCH have same blockId
    id: String = UUID.randomUUID.toString
) {
  def toLit: js.Object = js.Dynamic.literal(
    group = "edges",
    data = js.Dynamic.literal(
      id = id,
      label = label,
      source = source,
      target = target,
      dir = dir,
      blockId = if (blockId.isEmpty) id else blockId
    )
  )
}
