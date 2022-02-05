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
  // yellow, light_blue, blue, red
  // #ffd166, #8de4ff, #015692,
  val default = ColorScheme(
    startEndNode = NodeColor("#8de4ff", "#00799e", "#982b13"),
    declareNode = NodeColor("#015692", "#002a3a", "white"),
    assignNode = NodeColor("#002a3a", "#001117", "white"),
    ioNode = NodeColor("#015692", "#002a3a", "white"),
    loopNode = NodeColor("#ffd166", "#8f6400", "black"),
    callNode = NodeColor("#015692", "#002a3a", "white")
  )

case class NodeColor(
    fill: String = "white",
    border: String = "black",
    font: String = "black"
) {
  def graphvizColors =
    s""" fillcolor="$fill" color="$border" fontcolor="$font" """.trim
}
