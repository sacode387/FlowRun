package dev.sacode.flowrun

import scala.scalajs.js.annotation.*

@JSExportAll
@JSExportTopLevel("FlowRunTheme")
case class FlowRunTheme(
    fontName: String,
    startEndNode: Color,
    declareNode: Color,
    assignNode: Color,
    ioNode: Color,
    loopNode: Color,
    callNode: Color,
    commentNode: Color
) {
  def withFontName(fontName: String) = copy(fontName = fontName)
  def withStartEndNode(startEndNode: Color) = copy(startEndNode = startEndNode)
  def withDeclareNode(declareNode: Color) = copy(declareNode = declareNode)
  def withAssignNode(assignNode: Color) = copy(assignNode = assignNode)
  def withIoNode(ioNode: Color) = copy(ioNode = ioNode)
  def withLoopNode(loopNode: Color) = copy(loopNode = loopNode)
  def withCallNode(callNode: Color) = copy(callNode = callNode)
  def withCommentNode(commentNode: Color) = copy(commentNode = commentNode)
}

@JSExportTopLevel("FlowRunThemeObj")
object FlowRunTheme:
  @JSExport
  val default = FlowRunTheme(
    fontName = "Courier New",
    startEndNode = Color.lightBlue,
    declareNode = Color.blue,
    assignNode = Color.darkBlue,
    ioNode = Color.blue,
    loopNode = Color.yellow,
    callNode = Color.blue,
    commentNode = Color.grey
  )

@JSExportAll
@JSExportTopLevel("FlowRunColor")
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

object Color:
  val lightBlue = Color("#D9F6E5", "#3A8C68", "#1B4332")
  val blue = Color("#E0E7FF", "#6366F1", "#1E1B4B")
  val darkBlue = Color("#C7D2FE", "#4F46E5", "#1E1B4B")
  val yellow = Color("#FFF0C2", "#D38A11", "#713F12")
  val green = Color("#D9F6E5", "#258139", "#1B4332")
  val grey = Color("#E5E7EB", "#94A3B8", "#334155")
