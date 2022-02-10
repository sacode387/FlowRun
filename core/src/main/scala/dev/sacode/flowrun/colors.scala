package dev.sacode.flowrun

case class ColorScheme(
    startEndNode: Color,
    declareNode: Color,
    assignNode: Color,
    ioNode: Color,
    loopNode: Color,
    callNode: Color
)

object ColorScheme:
  // yellow, light_blue, blue, red
  // #ffd166, #8de4ff, #015692,
  val default = ColorScheme(
    startEndNode = Color.lightBlue,
    declareNode = Color.blue,
    assignNode = Color.darkBlue,
    ioNode = Color.blue,
    loopNode = Color.yellow,
    callNode = Color.blue
  )

case class Color(
    fill: String = "white",
    border: String = "black",
    font: String = "black"
) {
  def graphvizColors =
    s""" fillcolor="$fill" color="$border" fontcolor="$font" """.trim
}

object Color:
  val lightBlue = Color("#8de4ff", "#00799e", "#982b13")
  val blue = Color("#015692", "#002a3a", "white")
  val darkBlue = Color("#002a3a", "#001117", "white")
  val yellow = Color("#ffd166", "#8f6400", "black")
  val green = Color("#258139", "#214426", "white")
