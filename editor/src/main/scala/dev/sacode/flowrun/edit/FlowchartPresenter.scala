package dev.sacode.flowrun
package edit

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scalajs.js
import scalajs.js.JSConverters.*
import org.scalajs.dom
import reactify.*
import dev.sacode.flowrun.parse.*
import dev.sacode.flowrun.ast.*, Expression.Type, Statement.*
import dev.sacode.flowrun.eval.Interpreter.ExecMode

/*
- color==border_color, fillcolor==.. https://stackoverflow.com/questions/9106079/graphviz-how-to-change-border-color
 */
class FlowchartPresenter(
    programModel: ProgramModel,
    flowRunElements: FlowRunElements,
    flowRunTheme: FlowRunTheme,
    flowrunChannel: Channel[FlowRun.Event]
) {

  private val xIncrement = 120
  private val yIncrement = 90
  private val PxInInch = 96

  private val graphviz = d3
    .select(flowRunElements.drawArea)
    .graphviz(
      js.Dynamic.literal(
        zoom = true,
        fit = true
      )
    )

  loadCurrentFunction()

  def disable(mode: ExecMode): Unit =
    flowRunElements.drawArea.classList.add("flowrun--disabled")
    if mode == ExecMode.NORMAL then flowRunElements.runButton.classList.add("flowrun--disabled")
    flowRunElements.stopButton.classList.remove("flowrun--disabled")

  def enable(): Unit =
    flowRunElements.drawArea.classList.remove("flowrun--disabled")
    flowRunElements.runButton.classList.remove("flowrun--disabled")
    flowRunElements.stopButton.classList.add("flowrun--disabled")

  def clearErrors(): Unit =
    dom.window.document.querySelectorAll(".node ").foreach(_.classList.remove("flowrun--error"))

  def clearSelected(): Unit =
    dom.window.document.querySelectorAll(".flowrun--selected").foreach(_.classList.remove("flowrun--selected"))

  def highlightError(nodeId: String): Unit =
    if nodeId != null && nodeId.trim.nonEmpty then
      dom.window.document.querySelector(s""" .node[id^="$nodeId"] """).classList.add("flowrun--error")

  def highlightExecuting(nodeIdOpt: Option[String]): Unit =
    val cls = "flowrun--to-execute"
    dom.window.document.querySelectorAll(".node").foreach(_.classList.remove(cls))
    nodeIdOpt.foreach { nodeId =>
      val domNode = dom.window.document.querySelector(s""" .node[id^="${nodeId}"] """)
      if domNode != null && !js.isUndefined(domNode) then domNode.classList.add(cls)
    }

  def loadCurrentFunction(): Future[Unit] = {
    val p = Promise[Unit]()
    graphviz
      .engine("neato")
      .renderDot(
        funDOT,
        (gr: js.Dynamic) => {
          clearSelected()
          clearErrors()
          programModel.currentSelectedStmtId.foreach { id =>
            dom.window.document
              .querySelectorAll(s""" .node[id*="${id}"] """)
              .foreach(_.classList.add("flowrun--selected"))
          }
          flowrunChannel := FlowRun.Event.SvgMounted
          p.success(())
        }
      )
    p.future
  }

  def funDOT: String =
    s"""
    |digraph {
    |  bgcolor="transparent"
    |
    |  node [penwidth=0.5 fontsize="12" shape="box" style="filled" fontcolor="white" fontname="${flowRunTheme.fontName}"]
    |  edge [penwidth=2 fontsize="10" color="#0A1931" arrowsize=0.8 fontname="${flowRunTheme.fontName}"]
    |
    |#########
    |# NODES #
    |#########
    |${nodesDOT.indented(2)}
    |
    |#########
    |# EDGES #
    |#########
    |${edgesDOT.indented(2)}
    |
    |}
    |""".stripMargin

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
    import AST.newId

    // group puts elements of one branch in a straight line
    val group = s""" group="$blockId" """.trim

    val stmtId = s"${stmt.id}#${stmt.getClass.getSimpleName}"
    stmt match {
      case _: Begin =>
        val lbl = if programModel.currentFunction.isMain then "Begin" else programModel.currentFunction.label
        val tooltip = if programModel.currentFunction.isMain then "Begin" else programModel.currentFunction.verboseLabel
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$tooltip" shape="ellipse" ${flowRunTheme.startEndNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case retStmt: Return =>
        val lbl = if programModel.currentFunction.isMain then "End" else stmt.label
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$lbl" shape="ellipse" ${flowRunTheme.startEndNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Declare =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="${stmt.verboseLabel.toGraphvizLbl}" ${flowRunTheme.declareNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Assign =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$lbl" ${flowRunTheme.assignNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Input =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$lbl" shape="invtrapezium" ${flowRunTheme.ioNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Output =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$lbl" shape="trapezium" ${flowRunTheme.ioNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case _: Call =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$lbl" ${flowRunTheme.callNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case stmt: If =>
        val lbl = stmt.label.toGraphvizLbl

        val (trueNodeDOTs, trueOffsetX) = locally {
          val block = stmt.trueBlock
          val stmts = block.statements
          val x = posX + widthTrue(stmt, 0)
          val dots = stmts.foldLeft((List.empty[String], posY + 1)) { case ((prevDots, lastY), s) =>
            val dot = nodeDOT(s, block.id, x, lastY)
            (prevDots.appended(dot._1), dot._2 + 1)
          }
          (dots, x)
        }

        val (falseNodeDOTs, falseOffsetX) = locally {
          val block = stmt.falseBlock
          val stmts = block.statements
          val x = posX - widthFalse(stmt, 0)
          val dots = stmts.foldLeft((List.empty[String], posY + 1)) { case ((prevDots, lastY), s) =>
            val dot = nodeDOT(s, block.id, x, lastY)
            (prevDots.appended(dot._1), dot._2 + 1)
          }
          (dots, x)
        }

        val maxBranchY = trueNodeDOTs._2 max falseNodeDOTs._2

        val ifEndId = s"end_${stmt.id}"
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl, true)} $group 
              |  label="$lbl" tooltip="$lbl" shape="diamond" ${flowRunTheme.loopNode.graphvizColors}]
              |
              |true_dummy_up_${stmt.id} [${pos(trueOffsetX, posY)} shape=point width=0]
              |false_dummy_up_${stmt.id} [${pos(falseOffsetX, posY)} shape=point width=0]
              |
              |${trueNodeDOTs._1.mkString("\n")}
              |${falseNodeDOTs._1.mkString("\n")}
              |
              |true_dummy_down_${stmt.id} [ ${pos(trueOffsetX, maxBranchY)} shape=point width=0]
              |false_dummy_down_${stmt.id} [ ${pos(falseOffsetX, maxBranchY)} shape=point width=0]
              |
              |$ifEndId [id="$ifEndId#IfEnd" class="flowrun-not-selectable" ${pos(
               posX,
               maxBranchY
             )} $group ${flowRunTheme.loopNode.graphvizColors}
              |  label="" tooltip=" " shape="circle" fixedsize=true width=0.2 height=0.2]
              |
              |""".stripMargin
        (dot, maxBranchY)

      case stmt: While =>
        val lbl = stmt.label.toGraphvizLbl

        val (blockDOTs, trueOffsetX) = locally {
          val block = stmt.body
          val stmts = block.statements
          val x = posX + widthTrue(stmt, 0)
          val dots = stmts.foldLeft((List.empty[String], posY + 1)) { case ((prevDots, lastY), s) =>
            val dot = nodeDOT(s, block.id, x, lastY)
            (prevDots.appended(dot._1), dot._2 + 1)
          }
          (dots, x)
        }

        val falseOffsetX = posX - widthFalse(stmt, 0)

        val maxBranchY = blockDOTs._2

        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl, true)} $group 
              |  label="$lbl" tooltip="$lbl" shape="diamond" ${flowRunTheme.loopNode.graphvizColors}]
              |
              |true_dummy_up_${stmt.id} [${pos(trueOffsetX, posY)} shape=point width=0]
              |false_dummy_up_${stmt.id} [${pos(falseOffsetX, posY)} shape=point width=0]
              |
              |${blockDOTs._1.mkString("\n")}
              |
              |true_dummy_down_${stmt.id} [${pos(trueOffsetX, maxBranchY, -10)} shape=point width=0]
              |true_dummy_down_left_${stmt.id} [${pos(posX, maxBranchY, -10)} shape=point width=0]
              |false_dummy_down_${stmt.id} [${pos(falseOffsetX, maxBranchY)} shape=point width=0]
              |end_dummy_down_${stmt.id} [${pos(posX, maxBranchY)} $group shape=point width=0]
              |
              |""".stripMargin
        (dot, maxBranchY)

      case stmt: DoWhile =>
        val lbl = stmt.label.toGraphvizLbl
        val doWhileEndId = s"end_${stmt.id}"

        val blockDOTs = locally {
          val block = stmt.body
          val stmts = block.statements
          val dots = stmts.foldLeft((List.empty[String], posY + 1)) { case ((prevDots, lastY), s) =>
            val dot = nodeDOT(s, block.id, posX, lastY)
            (prevDots.appended(dot._1), dot._2 + 1)
          }
          dots
        }

        val trueOffsetX = posX + widthTrue(stmt, 0)

        val maxBranchY = blockDOTs._2

        val dot =
          s"""|$doWhileEndId [id="$doWhileEndId#DoWhileEndId" class="flowrun-not-selectable" ${pos(posX, posY)} $group 
              |  label="" tooltip=" " shape="circle" fixedsize=true width=0.2 height=0.2 ${flowRunTheme.loopNode.graphvizColors}]
              |
              |${blockDOTs._1.mkString("\n")}
              |
              |${stmt.id} [id="$stmtId" ${pos(posX, maxBranchY)} ${dimensions(lbl, true)} $group 
              |label="$lbl" tooltip="$lbl" shape="diamond" ${flowRunTheme.loopNode.graphvizColors}]
              |
              |true_dummy_up_${stmt.id} [${pos(trueOffsetX, posY)} shape=point width=0]
              |true_dummy_down_${stmt.id} [${pos(trueOffsetX, maxBranchY)} shape=point width=0]
              |
              |""".stripMargin
        (dot, maxBranchY)

      case stmt: ForLoop =>
        val lbl = stmt.label.toGraphvizLbl

        val (blockDOTs, trueOffsetX) = locally {
          val block = stmt.body
          val stmts = block.statements
          val x = posX + widthTrue(stmt, 0)
          val dots = stmts.foldLeft((List.empty[String], posY + 1)) { case ((prevDots, lastY), s) =>
            val dot = nodeDOT(s, block.id, x, lastY)
            (prevDots.appended(dot._1), dot._2 + 1)
          }
          (dots, x)
        }

        val falseOffsetX = posX - widthFalse(stmt, 0)

        val maxBranchY = blockDOTs._2

        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl, true)} $group 
              |  label="$lbl" tooltip="$lbl" shape="diamond" ${flowRunTheme.loopNode.graphvizColors}]
              |
              |true_dummy_up_${stmt.id} [${pos(trueOffsetX, posY)} shape=point width=0]
              |false_dummy_up_${stmt.id} [${pos(falseOffsetX, posY)} shape=point width=0]
              |
              |${blockDOTs._1.mkString("\n")}
              |
              |true_dummy_down_${stmt.id} [${pos(trueOffsetX, maxBranchY, -10)} shape=point width=0]
              |true_dummy_down_left_${stmt.id} [${pos(posX, maxBranchY, -10)} shape=point width=0]
              |false_dummy_down_${stmt.id} [${pos(falseOffsetX, maxBranchY)} shape=point width=0]
              |end_dummy_down_${stmt.id} [${pos(posX, maxBranchY)} $group shape=point width=0]
              |
              |""".stripMargin
        (dot, maxBranchY)

      case _: Block => ("", posY)

      case _: Comment =>
        val lbl = stmt.label.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$lbl" shape="parallelogram" ${flowRunTheme.commentNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)
    }
  }

  /* EDGES */
  private def edgesDOT: String = {
    val stmts = programModel.currentFunction.statements
    // draw edges starting from bottom up...
    val funId = programModel.currentFunction.id

    val statementsDot = stmts
      .zip(stmts.tail)
      .map { (prev, next) => edgeDOT(prev, funId, next.firstNodeId) }
      .mkString("\n")
    statementsDot
  }

  private def edgeDOT(
      stmt: Statement,
      blockId: String,
      nextStmtId: String,
      nextStmtDir: String = "n"
  ): String = {

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

      case _: Call =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""

      case stmt: If =>
        val ifEndId = s"end_${stmt.id}"
        val ifEndEdgeId = s"$ifEndId@${blockId}"
        val trueDummyUpId = s"true_dummy_up_${stmt.id}"
        val trueDummyDownId = s"true_dummy_down_${stmt.id}"
        val falseDummyUpId = s"false_dummy_up_${stmt.id}"
        val falseDummyDownId = s"false_dummy_down_${stmt.id}"

        val (trueEdgeDOTs, firstTrueNodeId, lastTrueNodeId) = locally {
          val block = stmt.trueBlock
          val stmts = block.statements
          val nextStmtIds = block.statements.drop(1).map(_.id) ++ List(s"true_dummy_down_${stmt.id}")

          val statementsDot = stmts
            .zip(nextStmtIds)
            .map { (prev, nextId) => edgeDOT(prev, block.id, nextId) }
            .mkString("\n")

          val first = stmts.headOption.map(_.firstNodeId).getOrElse(s"true_dummy_down_${stmt.id}")
          val last = stmts.lastOption.map(_.id).getOrElse(stmt.id)
          (statementsDot, first, last)
        }

        val (falseEdgeDOTs, firstFalseNodeId, lastFalseNodeId) = locally {
          val block = stmt.falseBlock
          val stmts = block.statements
          val nextStmtIds = block.statements.drop(1).map(_.id) ++ List(s"false_dummy_down_${stmt.id}")

          val statementsDot = stmts
            .zip(nextStmtIds)
            .map { (prev, nextId) => edgeDOT(prev, block.id, nextId) }
            .mkString("\n")

          val first = stmts.headOption.map(_.firstNodeId).getOrElse(s"false_dummy_down_${stmt.id}")
          val last = stmts.lastOption.map(_.id).getOrElse(stmt.id)
          (statementsDot, first, last)
        }

        s"""|## IF-TRUE
            |${stmt.id}:e -> $trueDummyUpId [id="${stmt.id}@${stmt.trueBlock.id}" ${edgeAttrs(
             trueDummyUpId
           )} taillabel="true" fontcolor="forestgreen"]
            |$trueDummyUpId -> $firstTrueNodeId:n [id="${stmt.id}@${stmt.trueBlock.id}" ${edgeAttrs(firstTrueNodeId)}]
            |
            |$trueEdgeDOTs
            |
            |$trueDummyDownId -> $ifEndId [id="${lastTrueNodeId}@${stmt.trueBlock.id}" ${edgeAttrs(ifEndId)}]
            |
            |
            |## IF-FALSE
            |${stmt.id}:w -> $falseDummyUpId [id="${stmt.id}@${stmt.falseBlock.id}" ${edgeAttrs(
             falseDummyUpId
           )} taillabel="false" fontcolor="red"]
            |$falseDummyUpId -> $firstFalseNodeId:n [id="${stmt.id}@${stmt.falseBlock.id}" ${edgeAttrs(
             firstFalseNodeId
           )}]
            |
            |$falseEdgeDOTs
            |
            |$falseDummyDownId -> $ifEndId [id="${lastFalseNodeId}@${stmt.falseBlock.id}" ${edgeAttrs(ifEndId)}]
            |
            |## IF-END
            |$ifEndId:s -> $nextStmtId:$nextStmtDir [id="$ifEndEdgeId" ${edgeAttrs(nextStmtId)}]
            |
            |""".stripMargin

      case stmt: While =>
        val trueDummyUpId = s"true_dummy_up_${stmt.id}"
        val trueDummyDownId = s"true_dummy_down_${stmt.id}"
        val trueDummyDownLeftId = s"true_dummy_down_left_${stmt.id}"
        val falseDummyUpId = s"false_dummy_up_${stmt.id}"
        val falseDummyDownId = s"false_dummy_down_${stmt.id}"
        val endDummyDownId = s"end_dummy_down_${stmt.id}"

        val (trueEdgeDOTs, firstTrueNodeId, lastTrueNodeId) = locally {
          val block = stmt.body
          val stmts = block.statements
          val nextStmtIds = block.statements.drop(1).map(_.id) ++ List(s"true_dummy_down_${stmt.id}")

          val statementsDot = stmts
            .zip(nextStmtIds)
            .map { (prev, nextId) => edgeDOT(prev, block.id, nextId) }
            .mkString("\n")

          val first = stmts.headOption.map(_.firstNodeId).getOrElse(s"true_dummy_down_${stmt.id}")
          val last = stmts.lastOption.map(_.id).getOrElse(stmt.id)
          (statementsDot, first, last)
        }

        s"""|## WHILE-TRUE
            |${stmt.id}:e -> $trueDummyUpId [id="${stmt.id}@${stmt.body.id}" ${edgeAttrs(
             trueDummyUpId
           )} taillabel="true" fontcolor="forestgreen"]
            |$trueDummyUpId -> $firstTrueNodeId:n [id="${stmt.id}@${stmt.body.id}" ${edgeAttrs(firstTrueNodeId)}]
            |
            |$trueEdgeDOTs
            |
            |$trueDummyDownId -> $trueDummyDownLeftId [id="${lastTrueNodeId}@${stmt.body.id}" ${edgeAttrs(
             trueDummyDownLeftId
           )}]
            |$trueDummyDownLeftId -> ${stmt.id}:s [id="${lastTrueNodeId}@${stmt.body.id}" ${edgeAttrs(stmt.id)}]
            |
            |## WHILE-FALSE
            |${stmt.id}:w -> $falseDummyUpId [id="${stmt.id}@${blockId}" ${edgeAttrs(
             falseDummyUpId
           )} taillabel="false" fontcolor="red"]
            |$falseDummyUpId -> $falseDummyDownId [id="${stmt.id}@${blockId}" ${edgeAttrs(falseDummyDownId)}]
            |$falseDummyDownId -> $endDummyDownId [id="${stmt.id}@${blockId}" ${edgeAttrs(endDummyDownId)}]
            |$endDummyDownId -> $nextStmtId [id="${stmt.id}@${blockId}" ${edgeAttrs(nextStmtId)}]
            |
            |""".stripMargin

      case stmt: DoWhile =>
        val doWhileEndId = s"end_${stmt.id}"
        val trueDummyUpId = s"true_dummy_up_${stmt.id}"
        val trueDummyDownId = s"true_dummy_down_${stmt.id}"

        val (trueEdgeDOTs, firstBlockNodeId, lastTrueNodeId) = locally {
          val block = stmt.body
          val stmts = block.statements
          val nextStmtIds = block.statements.appended(stmt).drop(1).map(_.id)

          val statementsDot = stmts
            .zip(nextStmtIds)
            .map { (prev, nextId) => edgeDOT(prev, block.id, nextId) }
            .mkString("\n")

          val first = stmts.headOption.map(_.firstNodeId).getOrElse(stmt.id)
          val last = stmts.lastOption.map(_.id).getOrElse(stmt.id)
          (statementsDot, first, last)
        }

        s"""|## DOWHILE-BODY
            |$doWhileEndId -> $firstBlockNodeId [id="${stmt.id}@${stmt.body.id}" ${edgeAttrs(firstBlockNodeId)} ]
            |$trueEdgeDOTs
            |
            |## DOWHILE-TRUE
            |${stmt.id}:e -> $trueDummyDownId [${edgeAttrs(
             trueDummyDownId
           )} taillabel="true" fontcolor="forestgreen" labelangle=90]
            |$trueDummyDownId -> $trueDummyUpId [${edgeAttrs(trueDummyUpId)}]
            |$trueDummyUpId -> ${doWhileEndId}:e [ ${edgeAttrs(stmt.id)}]
            |
            |## DOWHILE-FALSE
            |${stmt.id}:s -> $nextStmtId [id="${stmt.id}@${blockId}" ${edgeAttrs(
             nextStmtId
           )} taillabel="false" fontcolor="red" labeldistance=2 labelangle=-80]
            |
            |""".stripMargin

      case stmt: ForLoop =>
        val trueDummyUpId = s"true_dummy_up_${stmt.id}"
        val trueDummyDownId = s"true_dummy_down_${stmt.id}"
        val trueDummyDownLeftId = s"true_dummy_down_left_${stmt.id}"
        val falseDummyUpId = s"false_dummy_up_${stmt.id}"
        val falseDummyDownId = s"false_dummy_down_${stmt.id}"
        val endDummyDownId = s"end_dummy_down_${stmt.id}"

        val (trueEdgeDOTs, firstTrueNodeId, lastTrueNodeId) = locally {
          val block = stmt.body
          val stmts = block.statements
          val nextStmtIds = block.statements.drop(1).map(_.id) ++ List(s"true_dummy_down_${stmt.id}")

          val statementsDot = stmts
            .zip(nextStmtIds)
            .map { (prev, nextId) => edgeDOT(prev, block.id, nextId) }
            .mkString("\n")

          val first = stmts.headOption.map(_.firstNodeId).getOrElse(s"true_dummy_down_${stmt.id}")
          val last = stmts.lastOption.map(_.id).getOrElse(stmt.id)
          (statementsDot, first, last)
        }

        s"""|## FORLOOP-TRUE
            |${stmt.id}:e -> $trueDummyUpId [id="${stmt.id}@${stmt.body.id}" ${edgeAttrs(
             trueDummyUpId
           )} taillabel="true" fontcolor="forestgreen"]
            |$trueDummyUpId -> $firstTrueNodeId:n [id="${stmt.id}@${stmt.body.id}" ${edgeAttrs(firstTrueNodeId)}]
            |
            |$trueEdgeDOTs
            |
            |$trueDummyDownId -> $trueDummyDownLeftId [id="${lastTrueNodeId}@${stmt.body.id}" ${edgeAttrs(
             trueDummyDownLeftId
           )}]
            |$trueDummyDownLeftId -> ${stmt.id}:s [id="${lastTrueNodeId}@${stmt.body.id}" ${edgeAttrs(stmt.id)}]
            |
            |## FORLOOP-FALSE
            |${stmt.id}:w -> $falseDummyUpId [id="${stmt.id}@${blockId}" ${edgeAttrs(
             falseDummyUpId
           )} taillabel="false" fontcolor="red"]
            |$falseDummyUpId -> $falseDummyDownId [id="${stmt.id}@${blockId}" ${edgeAttrs(falseDummyDownId)}]
            |$falseDummyDownId -> $endDummyDownId [id="${stmt.id}@${blockId}" ${edgeAttrs(endDummyDownId)}]
            |$endDummyDownId -> $nextStmtId [id="${stmt.id}@${blockId}" ${edgeAttrs(nextStmtId)}]
            |
            |""".stripMargin

      case _: Block  => ""
      case _: Return => "" // no edges after return..
      case _: Comment =>
        val edgeId = s"${stmt.id}@$blockId"
        s"""${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$edgeId" ${edgeAttrs(nextStmtId)}]"""
    }
  }

  // Graphviz uses "mathematical" coordinates, with bottom left corner being (0,0)
  // https://stackoverflow.com/questions/55905661/how-to-force-neato-engine-to-reverse-node-order
  // it's easier here to have (0,0) at top-center (we just flip y axis that is..)
  private def pos(x: Int, y: Int, yOff: Int = 0): String =
    val xPx: Double = if x == 0 then 0 else px2Inch(x * xIncrement)
    val yPx = if y == 0 then 0 else px2Inch(y * yIncrement + yOff)
    val realY = 10_000 - yPx
    s""" pos="$xPx,$realY!" """.trim

  // neato requires inches for "pos"
  private def px2Inch(px: Int): Double =
    px.toDouble / PxInInch

  private def dimensions(label: String, luft: Boolean = false): String =
    val w = label.length * 0.11 + (if luft then 0.5 else 0.2)
    val width = w max 1
    val h = 0.4 + (if luft then 0.1 else 0)
    s"height=$h width=$width fixedsize=true"

  private def edgeAttrs(nextStmtId: String): String =
    val maybeNoArrow = Option.when(nextStmtId.contains("dummy"))("arrowhead=none").getOrElse("")
    s""" tailtooltip=" " edgetooltip=" " $maybeNoArrow """.trim

  //// Statement utils

  // umm, this logic is a bit hard to explain
  // best to concentrate on it visually
  // look nested ifs
  // depth is CRUCIAL
  private def widthFalse(stmt: Statement, depth: Int): Int = stmt match
    case stmt: If =>
      val wlMax = stmt.falseBlock.statements.map(s => widthFalse(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.falseBlock.statements.map(s => widthTrue(s, depth + 1)).maxOption.getOrElse(0)
      width(stmt) + 1 + (if depth == 0 then wrMax else wlMax + wrMax)
    case stmt: While   => width(stmt) + 1
    case stmt: DoWhile => width(stmt)
    case stmt: ForLoop => width(stmt) + 1
    case stmt          => width(stmt)

  private def widthTrue(statement: Statement, depth: Int): Int = statement match
    case stmt: If =>
      val wlMax = stmt.trueBlock.statements.map(s => widthFalse(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.trueBlock.statements.map(s => widthTrue(s, depth + 1)).maxOption.getOrElse(0)
      width(stmt) + 1 + (if depth == 0 then wlMax else wlMax + wrMax)
    case stmt: While =>
      val wlMax = stmt.body.statements.map(s => widthFalse(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.body.statements.map(s => widthTrue(s, depth + 1)).maxOption.getOrElse(0)
      width(stmt) + 1 + (if depth == 0 then wlMax else wlMax + wrMax)
    case stmt: DoWhile =>
      val wlMax = stmt.body.statements.map(s => widthFalse(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.body.statements.map(s => widthTrue(s, depth + 1)).maxOption.getOrElse(0)
      width(stmt) + 1 + (if depth == 0 then wrMax else wlMax + wrMax)
    case stmt: ForLoop =>
      val wlMax = stmt.body.statements.map(s => widthFalse(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.body.statements.map(s => widthTrue(s, depth + 1)).maxOption.getOrElse(0)
      width(stmt) + 1 + (if depth == 0 then wlMax else wlMax + wrMax)
    case stmt => width(stmt)

  private def width(stmt: Statement): Int =
    (stmt.label.toGraphvizLbl.length.toDouble / 19).floor.toInt

}
