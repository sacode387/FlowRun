package dev.sacode.flowrun
package edit

import scalajs.js
import scalajs.js.JSConverters.*
import org.scalajs.dom
import reactify.*
import dev.sacode.flowrun.parse.*
import dev.sacode.flowrun.edit.CtxMenu

class FunctionEditor(
    programModel: ProgramModel,
    flowrunChannel: Channel[FlowRun.Event],
    flowRunElements: FlowRunElements
) {

  private val PxInInch = 96

  loadCurrentFunction()

  CtxMenu(flowRunElements, programModel).init()

  def disable(): Unit = {}

  def enable(): Unit = {}

  def clearErrors(): Unit = {
    // TODO remove classes from higlighted nodes
    flowrunChannel := FlowRun.Event.Deselected
  }

  def loadCurrentFunction(): Unit = {
    val graphviz = d3
      .select(flowRunElements.drawArea)
      .graphviz(
        js.Dynamic.literal(
          zoom = false
        )
      )

    val dotSrc = funDOT
    //println(dotSrc)
    graphviz
      .engine("neato")
      .renderDot(dotSrc)
  }

  private def funDOT: String = {

    s"""
    |digraph {
    |  bgcolor="transparent"
    |
    |  node [penwidth=0.5 shape="box" style="filled" fontcolor="white" fontname="Courier New"]
    |  edge [penwidth=1.5 arrowsize=0.8]
    |
    |  $nodesDOT
    |
    |  $edgesDOT
    |
    |}
    |""".stripMargin

  }

  /* NODES */
  private def nodesDOT: String = {
    val stmts = programModel.currentFunction.statements
    val funId = programModel.currentFunction.id
    val dots = stmts.foldLeft((List.empty[String], 0)) { case ((prevDots, lastY), s) =>
      val dot = nodeDOT(s, funId, 0, lastY)
      (prevDots.appended(dot._1), dot._2 + 1)
    }

    dots._1.mkString("\n")
  }

  private def nodeDOT(
      stmt: Statement,
      blockId: String,
      posX: Int,
      posY: Int
  ): (String, Int) = {
    import Statement._
    import AST.newId

    // group puts elements of one branch in a straight line
    val group = s""" group="$blockId" """.trim

    val stmtId = s"${stmt.id}#${stmt.getClass.getSimpleName}"
    stmt match {
      case _: Begin =>
        val lbl = if programModel.currentFunction.isMain then "Begin" else programModel.currentFunction.label
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |label="$lbl" tooltip="$lbl" shape="ellipse" fillcolor="aqua" fontcolor="black"]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Return =>
        val lbl = if programModel.currentFunction.isMain then "End" else stmt.label
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |label="$lbl" tooltip="$lbl" shape="ellipse" fillcolor="aqua" fontcolor="black"]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Declare =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |label="$lbl" tooltip="$lbl" fillcolor="cornsilk" fontcolor="black"]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Assign =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |label="$lbl" tooltip="$lbl" fillcolor="red" fontcolor="black"]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Input =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |label="$lbl" tooltip="$lbl" shape="invtrapezium" fillcolor="mediumblue"]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Output =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |label="$lbl" tooltip="$lbl" shape="trapezium" fillcolor="mediumblue"]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Call =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |label="$lbl" tooltip="$lbl" fillcolor="mediumblue"]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case stmt: If =>
        val lbl = stmt.label.toGraphvizLbl
        val ifEndId = s"end_${stmt.id}"

        val (trueNodeDOTs, trueOffsetX) = locally {
          val block = stmt.trueBlock
          val stmts = block.statements

          val x = posX + widthRight(stmt, 0)

          val dots = stmts.foldLeft((List.empty[String], posY + 1)) { case ((prevDots, lastY), s) =>
            val dot = nodeDOT(s, block.id, x, lastY)
            (prevDots.appended(dot._1), dot._2 + 1)
          }
          (dots, x)
        }

        val (falseNodeDOTs, falseOffsetX) = locally {
          val block = stmt.falseBlock
          val stmts = block.statements

          val x = posX - widthLeft(stmt, 0)

          val dots = stmts.foldLeft((List.empty[String], posY + 1)) { case ((prevDots, lastY), s) =>
            val dot = nodeDOT(s, block.id, x, lastY)
            (prevDots.appended(dot._1), dot._2 + 1)
          }
          (dots, x)
        }

        val maxBranchY = trueNodeDOTs._2 max falseNodeDOTs._2

        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl, true)} $group 
              |label="$lbl" tooltip="$lbl" shape="diamond" fillcolor="yellow" fontcolor="black"]
              |
              |true_dummy_up_${stmt.id} [ ${pos(trueOffsetX, posY)} shape=point width=0]
              |false_dummy_up_${stmt.id} [ ${pos(falseOffsetX, posY)} shape=point width=0]
              |
              |${trueNodeDOTs._1.mkString("\n")}
              |${falseNodeDOTs._1.mkString("\n")}
              |
              |true_dummy_down_${stmt.id} [ ${pos(trueOffsetX, maxBranchY)} shape=point width=0]
              |false_dummy_down_${stmt.id} [ ${pos(falseOffsetX, maxBranchY)} shape=point width=0]
              |
              |$ifEndId [id="$ifEndId#IfEnd" ${pos(posX, maxBranchY)} $group 
              |label="" tooltip=" " shape="circle" fillcolor="black" fixedsize=true width=0.2 height=0.2]
              |
              |""".stripMargin
        (dot, maxBranchY)

      case _ => ("", posY)
    }
  }

  /* EDGES */
  private def edgesDOT: String = {
    val stmts = programModel.currentFunction.statements
    // draw edges starting from bottom up...
    val funId = programModel.currentFunction.id

    val statementsDot = stmts
      .zip(stmts.tail)
      .map { (prev, next) => edgeDOT(prev, funId, next.id) }
      .mkString("\n")
    statementsDot
  }

  private def edgeDOT(
      stmt: Statement,
      blockId: String,
      nextStmtId: String,
      nextStmtDir: String = "n"
  ): String = {

    import Statement._

    val stmtId = s"${stmt.id}#${stmt.getClass.getSimpleName}"
    stmt match {
      case Begin(isMain) =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""

      case _: Declare =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""

      case _: Assign =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""

      case _: Input =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""

      case _: Output =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""

      case _: Statement.Call =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""

      case stmt: If =>
        val ifEndId = s"end_${stmt.id}"
        val ifEndEdgeId = s"$ifEndId@${blockId}"
        val trueDummyUpId = s"true_dummy_up_${stmt.id}"
        val trueDummyDownId = s"true_dummy_down_${stmt.id}"
        val falseDummyUpId = s"false_dummy_up_${stmt.id}"
        val falseDummyDownId = s"false_dummy_down_${stmt.id}"

        val (trueEdgeDOTs, firstTrueNodeId) = locally {
          val block = stmt.trueBlock
          val stmts = block.statements
          val nextStmtIds = block.statements.drop(1).map(_.id) ++ List(s"true_dummy_down_${stmt.id}")

          val statementsDot = stmts
            .zip(nextStmtIds)
            .map { (prev, nextId) => edgeDOT(prev, block.id, nextId) }
            .mkString("\n")

          val first = stmts.headOption.map(_.id).getOrElse(s"true_dummy_down_${stmt.id}")
          (statementsDot, first)
        }

        val (falseEdgeDOTs, firstFalseNodeId) = locally {
          val block = stmt.falseBlock
          val stmts = block.statements
          val nextStmtIds = block.statements.drop(1).map(_.id) ++ List(s"false_dummy_down_${stmt.id}")

          val statementsDot = stmts
            .zip(nextStmtIds)
            .map { (prev, nextId) => edgeDOT(prev, block.id, nextId) }
            .mkString("\n")

          val first = stmts.headOption.map(_.id).getOrElse(s"false_dummy_down_${stmt.id}")
          (statementsDot, first)
        }

        s"""|## TRUE branch
            |${stmt.id}:e -> $trueDummyUpId [id="${stmt.id}@${stmt.trueBlock.id}" ${edgeAttrs(trueDummyUpId)} taillabel="true"]
            |$trueDummyUpId -> $firstTrueNodeId:n [id="${stmt.id}@${stmt.trueBlock.id}" ${edgeAttrs(firstTrueNodeId)}]
            |
            |$trueEdgeDOTs
            |
            |$trueDummyDownId -> $ifEndId [id="${stmt.id}@${stmt.trueBlock.id}" ${edgeAttrs(ifEndId)}]
            |
            |
            |## FALSE branch
            |${stmt.id}:w -> $falseDummyUpId [id="${stmt.id}@${stmt.falseBlock.id}" ${edgeAttrs(falseDummyUpId)} taillabel="false"]
            |$falseDummyUpId -> $firstFalseNodeId:n [id="${stmt.id}@${stmt.falseBlock.id}" ${edgeAttrs(firstFalseNodeId)}]
            |
            |$falseEdgeDOTs
            |
            |$falseDummyDownId -> $ifEndId [id="${stmt.id}@${stmt.falseBlock.id}" ${edgeAttrs(ifEndId)}]
            |
            |## IF-END
            |$ifEndId:s -> $nextStmtId:$nextStmtDir [id="$ifEndEdgeId" ${edgeAttrs(nextStmtId)}]
            |
            |""".stripMargin

      /*
      case stmt: While =>
        val lbl = stmt.label.toGraphvizLbl
        val falseEdgeId = s"$newId@${blockId}"

        val reverseTrueStmts = stmt.body.statements.reverse
        val trueStatementss =
          if reverseTrueStmts.isEmpty then ""
          else
            (List((reverseTrueStmts.head, stmt.id, "s")) ++
              reverseTrueStmts.tail
                .zip(reverseTrueStmts)
                .map((stmt, prevStmt) => (stmt, prevStmt.id, "n")))
              .map((s, n, dir) => getDOT(s, stmt.body.id, n, dir))
              .mkString("\n")
        val (firstTrueNodeId, trueDir) =
          if reverseTrueStmts.isEmpty then (stmt.id, "s")
          else (reverseTrueStmts.reverse.head.id, "n")

        s"""
           |${stmt.id} [id="${stmtId}" $group label="$lbl" tooltip="$lbl" $group ${dimensions(
          lbl,
          true
        )} shape="diamond" fillcolor="yellow" fontcolor="black"]
           |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$falseEdgeId" taillabel="false" ${edgeAttrs(nextStmtId)}]
           |
           |${stmt.id}:e -> $firstTrueNodeId:$trueDir [id="$newId@${stmt.body.id}" taillabel="true" ${edgeAttrs(nextStmtId)}]
           |
           |subgraph {
           |
           |  $trueStatementss
           |}
           |
           |""".stripMargin
       */
      case stmt: DoWhile => "" // TODO

      case _: Block  => ""
      case _: Return => "" // already drawn in loadCurrentFunction
    }
  }

  // Graphviz uses "mathematical" coordinates, with bottom left corner being (0,0)
  // https://stackoverflow.com/questions/55905661/how-to-force-neato-engine-to-reverse-node-order
  // it's easier here to have (0,0) at top-center (we just flip y axis that is..)
  private def pos(x: Int, y: Int): String =
    val xPx: Double = if x == 0 then 0 else px2Inch(x * 90)
    val yPx = if y == 0 then 0 else px2Inch(y * 60)
    val realY = 10_000 - yPx
    s""" pos="$xPx,$realY!" """.trim

  // neato requires inches for "pos"
  private def px2Inch(px: Int): Double =
    px.toDouble / PxInInch

  private def dimensions(label: String, luft: Boolean = false): String =
    val w = label.length * 0.15 + (if luft then 0.5 else 0)
    val width = w max 1
    val h = 0.3 + (if luft then 0.1 else 0)
    s"height=$h width=$width fixedsize=true"

  private def edgeAttrs(nextStmtId: String): String =
    val maybeNoArrow = Option.when(nextStmtId.contains("dummy"))("arrowhead=none").getOrElse("")
    s""" tailtooltip=" " edgetooltip=" " $maybeNoArrow """.trim

}
