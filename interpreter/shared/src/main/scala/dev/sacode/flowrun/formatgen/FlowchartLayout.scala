package dev.sacode.flowrun.formatgen

import dev.sacode.flowrun.ast.*
import Statement.*
import DotFormatting.*

// Recognizes compact choices and reserves horizontal space for all statement kinds.
// It plans geometry only; AST structure and editing IDs remain unchanged.
private[formatgen] final class FlowchartLayout(function: Function) {

  case class ChoiceLane(conditions: List[If], onTrue: Boolean) {
    def continuing(s: If): Block = if onTrue then s.trueBlock else s.falseBlock
    def outcome(s: If): Block = if onTrue then s.falseBlock else s.trueBlock
    val blocks: List[Block] = conditions.map(outcome) :+ continuing(conditions.last)

    // Reserve both sides of every outcome, including nested loops and lanes.
    private def radius(s: Statement, condition: Boolean = false): Double =
      nodeWidth(s.label.toGraphvizLbl, condition) * PxInInch / (2 * xIncrement)
    private def extent(s: Statement, left: Boolean): Double = s match {
      case _: If | _: While | _: DoWhile | _: ForLoop =>
        (if left then widthFalse(s, 1) else widthTrue(s, 1)) + 1.0
      case _ => radius(s)
    }
    private val extents: List[(Double, Double)] = blocks.zipWithIndex.map { (block, i) =>
      val conditionWidth = conditions.lift(i).map(s => radius(s, true)).getOrElse(0.4)
      val left = block.statements.map(s => extent(s, true)).maxOption.getOrElse(0.0)
      val right = block.statements.map(s => extent(s, false)).maxOption.getOrElse(0.0)
      (left max conditionWidth, right max conditionWidth)
    }
    val xs: List[Int] = extents.sliding(2).foldLeft(List(0)) { (xs, pair) =>
      // Use one ordinary node pitch for short labels; grow only for actual
      // node widths or nested bodies, with 18px of clearance between columns.
      val gap = if onTrue then pair.head._2 + pair.last._1 else pair.head._1 + pair.last._2
      val distance = (gap + 0.15).ceil.toInt
      xs :+ (xs.last + (if onTrue then distance else -distance))
    }
    val leftExtent: Double = xs.zip(extents).map((x, e) => e._1 - x).max
    val rightExtent: Double = xs.zip(extents).map((x, e) => x + e._2).max
    val left: Int = leftExtent.ceil.toInt
    val right: Int = rightExtent.ceil.toInt
  }

  case class ChoiceFork(root: If, trueLane: ChoiceLane, falseLane: ChoiceLane) {
    private val radius = nodeWidth(root.label.toGraphvizLbl, true) * PxInInch / (2 * xIncrement)
    val trueX: Int = (radius + trueLane.leftExtent + 0.15).ceil.toInt
    val falseX: Int = -(radius + falseLane.rightExtent + 0.15).ceil.toInt
    val left: Int = (falseLane.leftExtent - falseX).ceil.toInt
    val right: Int = (trueLane.rightExtent + trueX).ceil.toInt
    val sides: List[(Boolean, Block, ChoiceLane, Int)] = List(
      (true, root.trueBlock, trueLane, trueX),
      (false, root.falseBlock, falseLane, falseX)
    )
  }

  def fork(root: If): Option[ChoiceFork] =
    (nestedChoice(root.trueBlock), nestedChoice(root.falseBlock)) match {
      case (Some(yes), Some(no)) =>
        val trueLane = lane(yes).filter(_.onTrue).getOrElse(ChoiceLane(List(yes), true))
        val falseLane = lane(no).filter(!_.onTrue).getOrElse(ChoiceLane(List(no), false))
        Some(ChoiceFork(root, trueLane, falseLane))
      case _ => None
    }

  private def nestedChoice(block: Block): Option[If] = block.statements match {
    case (s: If) :: Nil => Some(s)
    case _              => None
  }

  // Reject the whole alternating chain, including its otherwise eligible tail.
  private lazy val alternatingChoiceIds: Set[String] = {
    def path(s: If): List[(If, Boolean)] = (nestedChoice(s.trueBlock), nestedChoice(s.falseBlock)) match {
      case (Some(child), None) => (s, true) :: path(child)
      case (None, Some(child)) => (s, false) :: path(child)
      case _                   => Nil
    }
    def visit(s: Statement): Set[String] = s match {
      case s: If =>
        val steps = path(s)
        val blocked =
          if steps.map(_._2).distinct.size > 1 then
            steps.map(_._1.id).toSet ++ steps.lastOption.toList.flatMap { (last, onTrue) =>
              nestedChoice(if onTrue then last.trueBlock else last.falseBlock).map(_.id)
            }
          else Set.empty[String]
        blocked ++ visit(s.trueBlock) ++ visit(s.falseBlock)
      case s: Block   => s.statements.flatMap(visit).toSet
      case s: While   => visit(s.body)
      case s: DoWhile => visit(s.body)
      case s: ForLoop => visit(s.body)
      case _          => Set.empty
    }
    function.statements.flatMap(visit).toSet
  }

  def lane(root: If): Option[ChoiceLane] = {
    if alternatingChoiceIds(root.id) then return None
    def collect(s: If, onTrue: Boolean): Option[List[If]] = {
      val next = nestedChoice(if onTrue then s.trueBlock else s.falseBlock)
      val opposite = nestedChoice(if onTrue then s.falseBlock else s.trueBlock)
      if opposite.nonEmpty then None // both qualify, or the direction changes
      else
        next match {
          case Some(child) => collect(child, onTrue).map(s :: _)
          case None        => Some(List(s))
        }
    }
    (nestedChoice(root.trueBlock), nestedChoice(root.falseBlock)) match {
      case (Some(_), None) => collect(root, true).map(ChoiceLane(_, true))
      case (None, Some(_)) => collect(root, false).map(ChoiceLane(_, false))
      case _               => None
    }
  }

  //// Statement utils

  // umm, this logic is a bit hard to explain
  // best to concentrate on it visually
  // look nested ifs
  // depth is CRUCIAL
  def widthFalse(stmt: Statement, depth: Int): Int = stmt match
    case stmt: If if fork(stmt).nonEmpty => fork(stmt).get.left
    case stmt: If if lane(stmt).nonEmpty => lane(stmt).get.left
    case stmt: If =>
      val wlMax = stmt.falseBlock.statements.map(s => widthFalse(s, depth + 1)).maxOption.getOrElse(0)
      val wrMax = stmt.falseBlock.statements.map(s => widthTrue(s, depth + 1)).maxOption.getOrElse(0)
      width(stmt) + 1 + (if depth == 0 then wrMax else wlMax + wrMax)
    case stmt: While   => width(stmt) + 1
    case stmt: DoWhile => width(stmt)
    case stmt: ForLoop => width(stmt) + 1
    case stmt          => width(stmt)

  def widthTrue(statement: Statement, depth: Int): Int = statement match
    case stmt: If if fork(stmt).nonEmpty => fork(stmt).get.right
    case stmt: If if lane(stmt).nonEmpty => lane(stmt).get.right
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
