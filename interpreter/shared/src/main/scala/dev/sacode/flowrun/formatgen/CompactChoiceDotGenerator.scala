package dev.sacode.flowrun.formatgen

import dev.sacode.flowrun.FlowRunTheme
import dev.sacode.flowrun.ast.*
import Statement.*
import DotFormatting.*

// Emits compact-choice nodes and routes every edge to its original insertion target.
// Ordinary statements in branch bodies are rendered by DotGenerator's callbacks.
private[formatgen] final class CompactChoiceDotGenerator(
    layout: FlowchartLayout,
    flowRunTheme: FlowRunTheme,
    nodeDOT: (Statement, String, Int, Int) => (String, Int),
    edgeDOT: (Statement, String, String, String) => String
) {
  import layout.{ChoiceLane, ChoiceFork}

  def supports(root: If): Boolean = layout.fork(root).nonEmpty || layout.lane(root).nonEmpty

  def nodes(root: If, blockId: String, x: Int, y: Int): (String, Int) =
    layout.fork(root) match {
      case Some(fork) => forkNodes(fork, blockId, x, y)
      case None =>
        val lane = layout.lane(root).get
        val body = choiceBody(lane, x, y)
        choiceNodes(lane, blockId, x, y, body, body.bottom)
    }

  def edges(root: If, blockId: String, nextId: String, nextDir: String): String =
    layout.fork(root) match {
      case Some(fork) => forkEdges(fork, blockId, nextId, nextDir)
      case None       => choiceEdges(layout.lane(root).get, blockId, nextId, nextDir)
    }

  private def forkNodes(fork: ChoiceFork, blockId: String, x: Int, y: Int): (String, Int) = {
    val bodies = fork.sides.map { (_, _, lane, offset) => choiceBody(lane, x + offset, y) }
    val bottom = bodies.map(_.bottom).max
    val children = fork.sides.zip(bodies).map { case ((_, block, lane, offset), body) =>
      choiceNodes(lane, block.id, x + offset, y, body, bottom)._1
    }
    val s = fork.root
    val label = s.label.toGraphvizLbl
    val root = s"""${s.id} [id="${s.id}#If" ${pos(x, y)} ${dimensions(label, true)} group="$blockId"
                 | label="$label" tooltip="$label" shape="diamond" ${flowRunTheme.loopNode.graphvizColors}]
                 |end_${s.id} [id="end_${s.id}#IfEnd" class="flowrun-not-selectable" ${pos(x, bottom)}
                 | group="$blockId" label="" tooltip=" " shape="circle" fixedsize=true width=0.2 height=0.2 ${flowRunTheme.loopNode.graphvizColors}]
                 |""".stripMargin
    ((List(root) ++ children).mkString("\n"), bottom)
  }

  private def forkEdges(fork: ChoiceFork, blockId: String, nextId: String, nextDir: String): String = {
    val root = fork.root
    val sides = fork.sides.map { (onTrue, block, lane, _) =>
      val child = lane.conditions.head
      val from = if onTrue then "e" else "w"
      val to = if onTrue then "w" else "e"
      s"""${root.id}:$from -> ${child.id}:$to [id="${root.id}@${block.id}" ${edgeAttrs(child.id)}
         | taillabel="$onTrue" fontcolor="${if onTrue then "forestgreen" else "red"}"]
         |${choiceEdges(lane, block.id, s"end_${root.id}", from, to)}
         |""".stripMargin
    }
    (sides :+ s"""end_${root.id}:s -> $nextId:$nextDir [id="end_${root.id}@$blockId" ${edgeAttrs(nextId)}]""")
      .mkString("\n")
  }

  private case class ChoiceBody(dot: String, bottom: Int)

  private def choiceBody(lane: ChoiceLane, x: Int, y: Int): ChoiceBody = {
    val bodies = lane.blocks.zip(lane.xs).map { (block, offset) =>
      block.statements.foldLeft((List.empty[String], y + 1)) { case ((dots, nextY), s) =>
        val (dot, endY) = nodeDOT(s, block.id, x + offset, nextY)
        (dots :+ dot, endY + 1)
      }
    }
    ChoiceBody(bodies.flatMap(_._1).mkString("\n"), bodies.map(_._2).max)
  }

  // All merges in one compact choice share the deepest outcome's baseline.
  private def choiceNodes(
      lane: ChoiceLane,
      blockId: String,
      x: Int,
      y: Int,
      body: ChoiceBody,
      bottom: Int
  ): (String, Int) = {
    val decisions = lane.conditions.zipWithIndex.map { (s, i) =>
      val parentBlock = if i == 0 then blockId else lane.continuing(lane.conditions(i - 1)).id
      val label = s.label.toGraphvizLbl
      s"""${s.id} [id="${s.id}#If" ${pos(x + lane.xs(i), y)} ${dimensions(label, true)} group="$parentBlock"
         |  label="$label" tooltip="$label" shape="diamond" ${flowRunTheme.loopNode.graphvizColors}]
         |end_${s.id} [id="end_${s.id}#IfEnd" class="flowrun-not-selectable" ${pos(x + lane.xs(i), bottom)}
         |  group="$parentBlock" label="" tooltip=" " shape="circle" fixedsize=true width=0.2 height=0.2 ${flowRunTheme.loopNode.graphvizColors}]
         |""".stripMargin
    }
    val last = lane.conditions.last
    val fallbackX = x + lane.xs.last
    val fallback = s"""choice_dummy_up_${last.id} [${pos(fallbackX, y)} shape=point width=0]
                      |choice_dummy_down_${last.id} [${pos(fallbackX, bottom)} shape=point width=0]""".stripMargin
    ((decisions ++ List(body.dot, fallback)).mkString("\n"), bottom)
  }

  private def choiceEdges(
      lane: ChoiceLane,
      blockId: String,
      nextId: String,
      nextDir: String,
      exitDir: String = "s"
  ): String = {
    def edge(from: String, to: String, id: String, label: String = ""): String =
      s"""$from -> $to [id="$id" ${edgeAttrs(to)} $label]"""
    def label(value: Boolean): String =
      s"""taillabel="$value" fontcolor="${if value then "forestgreen" else "red"}""""
    def bodyEdges(block: Block, end: String): String =
      block.statements
        .zip(block.statements.drop(1).map(_.firstNodeId) :+ end)
        .map((s, next) => edgeDOT(s, block.id, next, "n"))
        .mkString("\n")

    val side = if lane.onTrue then "e" else "w"
    val entry = if lane.onTrue then "w" else "e"
    val decisions = lane.conditions.zipWithIndex.map { (s, i) =>
      val outcome = lane.outcome(s)
      val continuation = lane.continuing(s)
      val end = s"end_${s.id}"
      val first = outcome.statements.headOption.map(_.firstNodeId).getOrElse(end)
      val exit = edge(s"${s.id}:s", s"$first:n", s"${s.id}@${outcome.id}", label(!lane.onTrue))
      val outcomeEdges = bodyEdges(outcome, end)
      val forward = lane.conditions.lift(i + 1) match {
        case Some(child) =>
          List(
            edge(s"${s.id}:$side", s"${child.id}:$entry", s"${s.id}@${continuation.id}", label(lane.onTrue)),
            edge(s"end_${child.id}:$entry", s"$end:$side", s"end_${child.id}@${continuation.id}")
          ).mkString("\n")
        case None =>
          val up = s"choice_dummy_up_${s.id}"
          val down = s"choice_dummy_down_${s.id}"
          val first = continuation.statements.headOption.map(_.firstNodeId).getOrElse(down)
          val last = continuation.statements.lastOption.map(_.id).getOrElse(s.id)
          List(
            edge(s"${s.id}:$side", up, s"${s.id}@${continuation.id}", label(lane.onTrue)),
            edge(up, s"$first:n", s"${s.id}@${continuation.id}"),
            bodyEdges(continuation, down),
            edge(down, end, s"$last@${continuation.id}")
          ).mkString("\n")
      }
      List(exit, outcomeEdges, forward).mkString("\n")
    }
    (decisions :+ edge(
      s"end_${lane.conditions.head.id}:$exitDir",
      s"$nextId:$nextDir",
      s"end_${lane.conditions.head.id}@$blockId"
    )).mkString("\n")
  }

}
