package dev.sacode.flowrun

case class ColorScheme(
    startEndNode: NodeColor,
    declareNode: NodeColor,
    assignNode: NodeColor,
    ioNode: NodeColor,
    loopNode: NodeColor,
    callNode: NodeColor
)

object ColorScheme:
  val default = ColorScheme(
    startEndNode = NodeColor("#577BC1", "#1e3052", "white"),
    declareNode = NodeColor("#f7f5b5", "#9a961", "black"),
    assignNode = NodeColor("#EA5C2B", "#65210a", "white"),
    ioNode = NodeColor("#344CB7", "#151e49", "white"),
    loopNode = NodeColor("#EBE645", "#6e6b0c", "black"),
    callNode = NodeColor("#000957", "#000423", "white")
  )

case class NodeColor(
    fill: String = "white",
    border: String = "black",
    font: String = "black"
) {
  def graphvizColors =
    s""" fillcolor="$fill" color="$border" fontcolor="$font" """.trim
}
