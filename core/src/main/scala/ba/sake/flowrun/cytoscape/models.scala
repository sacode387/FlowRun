package ba.sake.flowrun.cytoscape

import java.util.UUID
import scalajs.js
import js.annotation._
import org.scalajs.dom

@js.native
@JSGlobal
class cytoscape(props: js.Object) extends js.Object {
  def contextMenus(props: js.Object): Unit = js.native
  def add(item: js.Object): js.Object = js.native
  def remove(selector: String): js.Object = js.native
}

/* NODEs */

case class Node(
  label: String,
  tpe: String,
  w: Integer = null,
  h: Integer = null,
  startId: String = "", // if-metadata
  endId: String = "", // if-metadata
  rawExpr: String = "",
  rawName: String = "",
  rawTpe: String = "",
  id: String = UUID.randomUUID.toString
) {
  private val (ww: Int, hh: Int) =
    if (tpe == Node.If) (label.length * 11 + 25, 30)
    else if (tpe == Node.Dummy) (20, 20)
    else if (tpe == Node.IfEnd) (10, 10)
    else {
      val www = if (w == null) 35 max label.length * 11 else w
      val hhh = if (h == null) 25 else h
      (www, hhh)
    }

  private val maybeEditable = Option.when(tpe != Node.BeginEnd && tpe != Node.Dummy && tpe != Node.IfEnd)(" editable")

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
      rawTpe = rawTpe
    ),
    classes = tpe + maybeEditable.getOrElse("")
  )
}

object Node {
  val BeginEnd = "begin-end"
  val Output = "output"
  val Input = "input"
  val If = "if"
  val IfEnd = "if-end"
  val Dummy = "dummy" // empty node used just for layout dirty-fix
  val Declare = "declare"
  val Assign = "assign"
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
