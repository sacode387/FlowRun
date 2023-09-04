package dev.sacode.flowrun

import scala.scalajs.js.annotation.*

@JSExportAll
case class ColorScheme(
    fontName: String,
    startEndNode: Color,
    declareNode: Color,
    assignNode: Color,
    ioNode: Color,
    loopNode: Color,
    callNode: Color
) {
  def withFontName(fontName: String) = copy(fontName = fontName)
  def withStartEndNode(startEndNode: Color) = copy(startEndNode = startEndNode)
  def withDeclareNode(declareNode: Color) = copy(declareNode = declareNode)
  def withAssignNode(assignNode: Color) = copy(assignNode = assignNode)
  def withIoNode(ioNode: Color) = copy(ioNode = ioNode)
  def withLoopNode(loopNode: Color) = copy(loopNode = loopNode)
  def withCallNode(callNode: Color) = copy(callNode = callNode)
}

@JSExportTopLevel("FlowrunColorSchemeObj")
object ColorScheme:
  // yellow, light_blue, blue, red
  // #ffd166, #8de4ff, #015692,
  @JSExport
  val default = ColorScheme(
    fontName = "Courier New",
    startEndNode = Color.lightBlue,
    declareNode = Color.blue,
    assignNode = Color.darkBlue,
    ioNode = Color.blue,
    loopNode = Color.yellow,
    callNode = Color.blue
  )

@JSExportAll
case class Color(
    fill: String = "white",
    border: String = "black",
    font: String = "black"
) {
  def graphvizColors =
    s""" fillcolor="$fill" color="$border" fontcolor="$font" """.trim

  def withFill(fill: String) = copy(fill = fill)
  def withBorder(border: String) = copy(border = border)
  def withFont(font: String) = copy(font = font)
}

@JSExportTopLevel("FlowrunColorObj")
object Color:
  val lightBlue = Color("#8de4ff", "#00799e", "#982b13")
  val blue = Color("#015692", "#002a3a", "white")
  val darkBlue = Color("#002a3a", "#001117", "white")
  val yellow = Color("#ffd166", "#8f6400", "black")
  val green = Color("#258139", "#214426", "white")
