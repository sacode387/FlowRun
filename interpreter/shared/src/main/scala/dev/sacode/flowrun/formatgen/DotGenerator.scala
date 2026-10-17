package dev.sacode.flowrun
package formatgen

import dev.sacode.flowrun.parse.*
import dev.sacode.flowrun.ast.*, Statement.*
import DotFormatting.*

class DotGenerator(function: Function, flowRunTheme: FlowRunTheme) {

  private val layout = FlowchartLayout(function)
  private val choices = CompactChoiceDotGenerator(layout, flowRunTheme, nodeDOT, edgeDOT)
  import layout.{widthFalse, widthTrue}

  def generate: String =
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
    val stmts = function.statements
    val funId = function.id
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
    // group puts elements of one branch in a straight line
    val group = s""" group="$blockId" """.trim

    val stmtId = s"${stmt.id}#${stmt.getClass.getSimpleName}"
    stmt match {
      case _: Begin =>
        val lbl = if function.isMain then "Begin" else function.label.toGraphvizLbl
        val tooltip = if function.isMain then "Begin" else function.verboseLabel.toGraphvizLbl
        val dot =
          s"""|${stmt.id} [id="$stmtId" ${pos(posX, posY)} ${dimensions(lbl)} $group 
              |  label="$lbl" tooltip="$tooltip" shape="ellipse" ${flowRunTheme.startEndNode.graphvizColors}]
              |""".stripMargin.replaceAll("\n", " ")
        (dot, posY)

      case retStmt: Return =>
        val lbl = if function.isMain then "End" else stmt.label.toGraphvizLbl
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

      case stmt: If if choices.supports(stmt) =>
        choices.nodes(stmt, blockId, posX, posY)

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
    val stmts = function.statements
    // draw edges starting from bottom up...
    val funId = function.id

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

      case stmt: If if choices.supports(stmt) =>
        choices.edges(stmt, blockId, nextStmtId, nextStmtDir)

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

}
