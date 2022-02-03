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
  // https://coolors.co/52aa5e-ffd166-725752-015692-fb4d3d
  // https://stackoverflow.com/questions/11867545/change-text-color-based-on-brightness-of-the-covered-background-area
  // green, yellow, light_blue, blue, red
  // #90ee90, #ffd166, #8de4ff, #015692, #fb4d3d
  val default = ColorScheme(
    startEndNode = NodeColor("#8de4ff", "#00799e", "#982b13"),
    declareNode = NodeColor("#90ee90", "#214426", "white"),
    assignNode = NodeColor("#fb4d3d", "#7a0d03", "white"),
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
