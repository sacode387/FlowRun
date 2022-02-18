package dev.sacode.flowrun.edit

import org.scalajs.dom
import reactify.*
import org.getshaka.nativeconverter.fromJson
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.FlowRunElements
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.{AST, Statement, Expression}, Statement.*

class CtxMenu(programModel: ProgramModel, flowRunElements: FlowRunElements, flowrunChannel: Channel[FlowRun.Event]) {

  /** used for delete */
  private var nodeId = ""

  private var afterId = ""
  private var blockId = ""

  private val DeleteableNodeTypes =
    Set("Declare", "Assign", "Input", "Output", "Call", "If", "While", "DoWhile", "ForLoop")

  private val edgeContextMenu =
    flowRunElements.mountElem.querySelector(".flowrun-edge-context-menu").asInstanceOf[dom.html.Element]
  private val nodeContextMenu =
    flowRunElements.mountElem.querySelector(".flowrun-node-context-menu").asInstanceOf[dom.html.Element]

  def init(): Unit =
    attachListeners()

  def handleClick(x: Double, y: Double, n: dom.svg.Element): Unit = {
    // here we know which EDGE is clicked
    // we save relevant ids, and then use them when a button is clicked
    hideAllMenus()

    val canActivate = setEdgeIds(n.id)
    if canActivate then
      edgeContextMenu.style.left = s"${x}px"
      edgeContextMenu.style.top = s"${y}px"
      edgeContextMenu.classList.add("active")
  }

  def handleRightClick(event: dom.MouseEvent, nodeId: String, nodeTpe: String): Unit = {
    // here we know which NODE is right-clicked
    // we save relevant ids, and then use them when delete is clicked
    hideAllMenus()
    this.nodeId = nodeId
    if DeleteableNodeTypes(nodeTpe) then
      nodeContextMenu.style.left = s"${event.clientX}px"
      nodeContextMenu.style.top = s"${event.clientY}px"
      nodeContextMenu.classList.add("active")
  }

  def hideAllMenus(): Unit =
    dom.document
      .getElementsByClassName("flowrun-context-menu")
      .foreach { e =>
        e.asInstanceOf[dom.html.Element].classList.remove("active")
      }

  private def setEdgeIds(edgeId: String): Boolean = {
    val parts = edgeId.split("@")
    if parts.length != 2 then return false

    afterId = parts(0)
    blockId = parts(1)

    if afterId.startsWith("end_") then afterId = afterId.drop("end_".length)
    else if afterId.startsWith("true_dummy_up_") then afterId = afterId.drop("true_dummy_up_".length)
    else if afterId.startsWith("true_dummy_down_") then afterId = afterId.drop("true_dummy_down_".length)
    else if afterId.startsWith("false_dummy_up_") then afterId = afterId.drop("false_dummy_up_".length)
    else if afterId.startsWith("false_dummy_down_") then afterId = afterId.drop("false_dummy_down_".length)

    true
  }

  private def attachListeners(): Unit = {

    val copyButton = nodeContextMenu.querySelector(".flowrun-copy-stmt").asInstanceOf[dom.html.Element]
    val deleteButton = nodeContextMenu.querySelector(".flowrun-delete").asInstanceOf[dom.html.Element]

    val pasteButton = edgeContextMenu.querySelector(".flowrun-paste-stmt").asInstanceOf[dom.html.Element]
    val addDeclareButton = edgeContextMenu.querySelector(".flowrun-add-declare").asInstanceOf[dom.html.Element]
    val addAssignButton = edgeContextMenu.querySelector(".flowrun-add-assign").asInstanceOf[dom.html.Element]
    val addInputButton = edgeContextMenu.querySelector(".flowrun-add-input").asInstanceOf[dom.html.Element]
    val addOutputButton = edgeContextMenu.querySelector(".flowrun-add-output").asInstanceOf[dom.html.Element]
    val addCallButton = edgeContextMenu.querySelector(".flowrun-add-call").asInstanceOf[dom.html.Element]
    val addIfButton = edgeContextMenu.querySelector(".flowrun-add-if").asInstanceOf[dom.html.Element]
    val addWhileButton = edgeContextMenu.querySelector(".flowrun-add-while").asInstanceOf[dom.html.Element]
    val addDoWhileButton = edgeContextMenu.querySelector(".flowrun-add-do-while").asInstanceOf[dom.html.Element]
    val addForLoopButton = edgeContextMenu.querySelector(".flowrun-add-for").asInstanceOf[dom.html.Element]

    // node buttons
    copyButton.addEventListener(
      "click",
      (event: dom.MouseEvent) =>
        val stmtJson = programModel.findStatement(nodeId).toJson
        dom.window.navigator.clipboard.writeText(stmtJson)
        flowrunChannel := FlowRun.Event.Deselected
    )

    deleteButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => programModel.delete(nodeId)
    )

    // edge buttons
    pasteButton.addEventListener(
      "click",
      (event: dom.MouseEvent) =>
        dom.window.navigator.clipboard.readText().`then` { stmtJson =>
          val newStmt = stmtJson.fromJson[Statement].duplicated
          addStatement(newStmt)
        }
    )

    addDeclareButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(Declare(AST.newId, "x", Expression.Type.Integer, None))
    )

    addAssignButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(Assign(AST.newId, "x", "19"))
    )

    addInputButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(Input(AST.newId, "x"))
    )

    addOutputButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(Output(AST.newId, "\"output\""))
    )

    addCallButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(Call(AST.newId, "fun1()"))
    )

    addIfButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(If(AST.newId, "true", Block(AST.newId), Block(AST.newId)))
    )

    addWhileButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(While(AST.newId, "true", Block(AST.newId)))
    )

    addDoWhileButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(DoWhile(AST.newId, "true", Block(AST.newId)))
    )

    addForLoopButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => addStatement(ForLoop(AST.newId, "i", "0", "1", "10", Block(AST.newId)))
    )
  }

  private def addStatement(stmt: Statement): Unit =
    programModel.addStmt(stmt, afterId, blockId)

}
