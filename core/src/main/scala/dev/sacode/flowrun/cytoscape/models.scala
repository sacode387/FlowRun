package dev.sacode.flowrun.cytoscape

import java.util.UUID
import scalajs.js
import js.annotation.*
import org.scalajs.dom

object Node {
  val Begin = "Begin"
  val End = "End"
  val Start = "Function"
  val Return = "Return"
  val Output = "Output"
  val Input = "Input"

  val If = "If"
  val IfEnd = "IfEnd"

  val While = "While"
  val WhileEnd = "WhileEnd"

  val DoWhile = "DoWhile"
  val DoWhileEnd = "DoWhileEnd"

  val Dummy = "Dummy" // empty node used just for layout dirty-fix
  val Declare = "Declare"
  val Assign = "Assign"
  val Call = "Call"

  val Editable = "Editable"
  val Removable = "Removable"
}
