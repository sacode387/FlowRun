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

  loadCurrentFunction()

  CtxMenu(flowRunElements, programModel, this).init()

  def disable(): Unit = {}

  def enable(): Unit = {}

  def clearErrors(): Unit =
    flowrunChannel := FlowRun.Event.SyntaxSuccess

  def loadCurrentFunction(): Unit = {
    val graphviz = d3
      .select(flowRunElements.drawArea)
      .graphviz(
        js.Dynamic.literal(
          zoom = false
        )
      )

    val stmts = programModel.currentFunction.statements
    //println(programModel.ast.toJson)

    import Statement._

    // load nodes starting from bottom up...
    val mainGroup = programModel.currentFunction.name
    val lastStmt = stmts.last
    val reverseStmts = stmts.reverse
    val statementsDot = reverseStmts.tail
      .zip(reverseStmts)
      .map((stmt, prevStmt) => getDOT(stmt, "", prevStmt.id, mainGroup))
      .mkString("\n")

    // colors: https://graphviz.org/doc/info/colors.html#svg
    val endLabel = Option.when(programModel.currentFunction.tpe == Expression.Type.Void)("End").getOrElse(lastStmt.label)
    val dotSrc = s"""
    digraph {
        nodesep=0.65
        ranksep=0.35
        bgcolor="transparent"
        splines="spline"
        #splines=ortho
       

        node [shape="box" style="filled" fillcolor="white" height=0.3 penwidth="0.5" margin="0.1,0" fontcolor="white" fontname="Courier New"]
        edge [penwidth=1.5 arrowsize=0.8]
        
        ${lastStmt.id} [id="${lastStmt.id}" label="${endLabel}" tooltip="${endLabel}" group="$mainGroup" height=0.3 shape="ellipse" fillcolor="aqua" fontcolor="black"]

        $statementsDot
    }
    """

    println(dotSrc)

    graphviz.renderDot(dotSrc)
  }

  import Statement._
  import AST.newId

  private def getDOT(
      stmt: Statement,
      blockId: String,
      nextStmtId: String,
      groupName: String,
      nextStmtDir: String = "n"
  ): String = {
    
    // group puts elements of one branch in a straight line
    val group = s"""group="$groupName"""" 
    stmt match {
      case Begin(isMain) =>
        val lbl = if isMain then stmt.label else programModel.currentFunction.label
        s"""
        |${stmt.id} [id="${stmt.id}" label="$lbl" tooltip="$lbl" shape="ellipse" fillcolor="aqua" fontcolor="black" ${dimensions(stmt)}]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Declare =>
        val lbl = stmt.label.toGraphvizLbl
        s"""
        |${stmt.id} [id="${stmt.id}" $group label="$lbl" tooltip="$lbl" fillcolor="cornsilk" fontcolor="black" margin="0.05" ${dimensions(stmt)}]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Assign =>
        val lbl = stmt.label.toGraphvizLbl
        s"""
        |${stmt.id} [id="${stmt.id}" $group label="$lbl" tooltip="$lbl" fillcolor="red" ${dimensions(stmt)}]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Input =>
        val lbl = stmt.label.toGraphvizLbl
        s"""
        |${stmt.id} [id="${stmt.id}" $group label="$lbl"  tooltip="$lbl" shape="invtrapezium" fillcolor="mediumblue" ${dimensions(stmt)}]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Output =>
        val lbl = stmt.label.toGraphvizLbl
        s"""
        |${stmt.id} [id="${stmt.id}" $group label="$lbl" tooltip="$lbl" shape="trapezium" fillcolor="mediumblue" ${dimensions(stmt)}]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Statement.Call =>
        val lbl = stmt.label.toGraphvizLbl
        s"""
        |${stmt.id} [id="${stmt.id}" $group label="$lbl" tooltip="$lbl" shape="trapezium" fillcolor="mediumblue" ${dimensions(stmt)}]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin

      case stmt: If =>
        val lbl = stmt.label.toGraphvizLbl
        val ifEndId = s"end_${stmt.id}"

        val reverseTrueStmts = stmt.trueBlock.statements.reverse
        val trueStatementss =
          if reverseTrueStmts.isEmpty then ""
          else
            (List((reverseTrueStmts.head, ifEndId, "e")) ++
              reverseTrueStmts.tail
                .zip(reverseTrueStmts)
                .map((stmt, prevStmt) => (stmt, prevStmt.id, "n")))
              .map((s, n, dir) => getDOT(s, stmt.trueBlock.id, n, s"true_${stmt.id}", dir))
              .mkString("\n")
        val (firstTrueNodeId, trueDir) =
          if reverseTrueStmts.isEmpty 
          then (ifEndId, "e")
          else (reverseTrueStmts.reverse.head.id, "n")

        val reverseFalseStmts = stmt.falseBlock.statements.reverse
        val falseStatementss =
          if reverseFalseStmts.isEmpty then ""
          else
            (List((reverseFalseStmts.head, ifEndId, "w")) ++
              reverseFalseStmts.tail
                .zip(reverseFalseStmts)
                .map((stmt, prevStmt) => (stmt, prevStmt.id, "n")))
              .map((s, n, dir) => getDOT(s, stmt.falseBlock.id, n, s"false_${stmt.id}", dir))
              .mkString("\n")
        val (firstFalseNodeId, falseDir) =
          if reverseFalseStmts.isEmpty
          then (ifEndId, "w")
          else (reverseFalseStmts.reverse.head.id, "n")

        s"""
          |$ifEndId [id="$ifEndId" $group label="" tooltip=" " shape="circle" fillcolor="black" fixedsize=true width=0.2 height=0.2 ]
          |$ifEndId:s -> $nextStmtId:$nextStmtDir [id="$newId"]
          |
          |${stmt.id} [id="${stmt.id}" $group label="$lbl" tooltip="$lbl" $group ${dimensions(stmt, true)} shape="diamond" fillcolor="yellow" fontcolor="black"]
          |
          |${stmt.id}:e -> $firstTrueNodeId:$trueDir [id="${stmt.trueBlock.id}" taillabel="true"]
          |${stmt.id}:w -> $firstFalseNodeId:$falseDir [id="${stmt.falseBlock.id}" taillabel="false"]
          |
          |$trueStatementss
          |$falseStatementss
          |""".stripMargin

      case stmt: While => "" // TODO
      case stmt: DoWhile => "" // TODO

      case _: Statement.Block => ""
      case _: Statement.Return => "" // already drawn in loadCurrentFunction
    }
  }

  private def dimensions(stmt: Statement, luft: Boolean = false): String =
    val w = stmt.label.toGraphvizLbl.length * 0.15 + (if luft then 0.5 else 0)
    val width = w max 1
    val h = 0.3  + (if luft then 0.1 else 0)
    s"height=$h width=$width fixedsize=true"

}
