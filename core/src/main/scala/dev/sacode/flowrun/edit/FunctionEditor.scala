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
       

        node [shape="box" style="filled" fillcolor="white" penwidth="0.5" margin="0.1,0" fontcolor="white" fontname="Courier New"]
        edge [penwidth=2]
        
        ${lastStmt.id} [id="${lastStmt.id}" label="${endLabel}" group="$mainGroup" shape="ellipse" fillcolor="aqua" fontcolor="black"]

        $statementsDot
    }
    """

    //println(dotSrc)

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
    val group = ""// TODO remove ??? s"""group="$groupName""""
    stmt match {
      case Begin(isMain) =>
        val lbl = if isMain then stmt.label else programModel.currentFunction.label
        s"""
        |${stmt.id} [id="${stmt.id}" label="${lbl}" shape="ellipse" fillcolor="aqua" fontcolor="black"]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Declare =>
        s"""
        |${stmt.id} [id="${stmt.id}" label="${stmt.label}" $group fillcolor="cornsilk" fontcolor="black" margin="0.05" ]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Assign =>
        s"""
        |${stmt.id} [id="${stmt.id}" label="${stmt.label}" $group fillcolor="red"]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Input =>
        s"""
        |${stmt.id} [id="${stmt.id}" label="${stmt.label}" $group shape="invtrapezium" fillcolor="mediumblue"]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Output =>
        s"""
        |${stmt.id} [id="${stmt.id}" label="${stmt.label}" $group shape="trapezium" fillcolor="mediumblue"]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin
      case _: Statement.Call =>
        s"""
        |${stmt.id} [id="${stmt.id}" label="${stmt.label}" $group shape="trapezium" fillcolor="mediumblue"]
        |${stmt.id}:s -> $nextStmtId:$nextStmtDir [id="$newId"]
        |""".stripMargin

      case stmt: If =>
        val ifEndId = newId

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
          |$ifEndId [id="$ifEndId" label="" $group shape="circle" fillcolor="black" fixedsize=true width=0.3 height=0.3]
          |$ifEndId:s -> $nextStmtId:$nextStmtDir [id="$newId"]
          |
          |${stmt.id} [id="${stmt.id}" label="${stmt.label}" $group shape="diamond" fillcolor="yellow" fontcolor="black"]
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

}
