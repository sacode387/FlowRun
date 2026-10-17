package dev.sacode.flowrun.formatgen

private[formatgen] object DotFormatting {
  val xIncrement = 120
  private val yIncrement = 90
  val PxInInch = 96

  // Graphviz uses "mathematical" coordinates, with bottom left corner being (0,0)
  // https://stackoverflow.com/questions/55905661/how-to-force-neato-engine-to-reverse-node-order
  // it's easier here to have (0,0) at top-center (we just flip y axis that is..)
  def pos(x: Int, y: Int, yOff: Int = 0): String =
    val xPx: Double = if x == 0 then 0 else px2Inch(x * xIncrement)
    val yPx = if y == 0 then 0 else px2Inch(y * yIncrement + yOff)
    val realY = 10_000 - yPx
    s""" pos="$xPx,$realY!" """.trim

  // neato requires inches for "pos"
  private def px2Inch(px: Int): Double =
    px.toDouble / PxInInch

  def nodeWidth(label: String, luft: Boolean = false): Double =
    (label.length * 0.11 + (if luft then 0.5 else 0.2)) max 1

  def dimensions(label: String, luft: Boolean = false): String =
    val width = nodeWidth(label, luft)
    val h = 0.4 + (if luft then 0.1 else 0)
    s"height=$h width=$width fixedsize=true"

  def edgeAttrs(nextStmtId: String): String =
    val maybeNoArrow = Option.when(nextStmtId.contains("dummy"))("arrowhead=none").getOrElse("")
    s""" tailtooltip=" " edgetooltip=" " $maybeNoArrow """.trim

  extension (str: String) {
    def toGraphvizLbl: String =
      val escaped = str.replace("\"", "\\\"")
      if escaped.endsWith("\\") then s"${escaped}\\" else escaped
  }
}
