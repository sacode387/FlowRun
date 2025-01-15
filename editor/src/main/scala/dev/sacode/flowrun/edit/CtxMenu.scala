package dev.sacode.flowrun.edit

import scala.util.control.NonFatal
import org.scalajs.dom
import reactify.*
import ba.sake.tupson.*
import dev.sacode.flowrun.toastify
import dev.sacode.flowrun.ToastifyOptions
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.FlowRunElements
import dev.sacode.flowrun.FlowRun
import dev.sacode.flowrun.ast.{AST, Statement, Expression}, Statement.*
import dev.sacode.flowrun.Color

class CtxMenu(programModel: ProgramModel, flowRunElements: FlowRunElements, flowrunChannel: Channel[FlowRun.Event]) {

  /* when selecting/deleting node */
  private var nodeId = ""

  /* when adding new node */
  private var blockId = ""
  private var afterId = ""

  private val DeleteableNodeTypes =
    Set("Declare", "Assign", "Input", "Output", "Call", "If", "While", "DoWhile", "ForLoop", "Comment")

  private val edgeContextMenu =
    flowRunElements.mountElem.querySelector(".flowrun-edge-context-menu").asInstanceOf[dom.html.Element]
  private val nodeContextMenu =
    flowRunElements.mountElem.querySelector(".flowrun-node-context-menu").asInstanceOf[dom.html.Element]

  // move menus to ROOT, <body>
  // so when you scroll it stays there..
  dom.document.body.appendChild(edgeContextMenu)
  dom.document.body.appendChild(nodeContextMenu)

  private val copyButton = nodeContextMenu.querySelector(".flowrun-copy-stmt").asInstanceOf[dom.html.Element]
  private val deleteButton = nodeContextMenu.querySelector(".flowrun-delete").asInstanceOf[dom.html.Element]

  private val pasteButton = edgeContextMenu.querySelector(".flowrun-paste-stmt").asInstanceOf[dom.html.Button]
  private val addDeclareButton = edgeContextMenu.querySelector(".flowrun-add-declare").asInstanceOf[dom.html.Element]
  private val addAssignButton = edgeContextMenu.querySelector(".flowrun-add-assign").asInstanceOf[dom.html.Element]
  private val addInputButton = edgeContextMenu.querySelector(".flowrun-add-input").asInstanceOf[dom.html.Element]
  private val addOutputButton = edgeContextMenu.querySelector(".flowrun-add-output").asInstanceOf[dom.html.Element]
  private val addCallButton = edgeContextMenu.querySelector(".flowrun-add-call").asInstanceOf[dom.html.Element]
  private val addIfButton = edgeContextMenu.querySelector(".flowrun-add-if").asInstanceOf[dom.html.Element]
  private val addWhileButton = edgeContextMenu.querySelector(".flowrun-add-while").asInstanceOf[dom.html.Element]
  private val addDoWhileButton = edgeContextMenu.querySelector(".flowrun-add-do-while").asInstanceOf[dom.html.Element]
  private val addForLoopButton = edgeContextMenu.querySelector(".flowrun-add-for").asInstanceOf[dom.html.Element]
  private val addCommentButton = edgeContextMenu.querySelector(".flowrun-add-comment").asInstanceOf[dom.html.Element]

  def init(): Unit =
    attachListeners()

  def handleEdgeRightClick(event: dom.MouseEvent, n: dom.svg.Element): Unit = {
    // here we know which EDGE is clicked
    // we save relevant ids, and then use them when a button is clicked
    hideAllMenus()

    val canActivate = setEdgeIds(n.id)
    if canActivate then
      val x = event.pageX
      val y = event.pageY
      edgeContextMenu.style.left = s"${x}px"
      edgeContextMenu.style.top = s"${y}px"
      edgeContextMenu.classList.add("active")
      dom.window.navigator.clipboard.readText().`then` { copiedText =>
        val hasText = Option(copiedText).getOrElse("").trim.nonEmpty
        pasteButton.disabled = !hasText
      }
  }

  def handleNodeRightClick(event: dom.MouseEvent, nodeId: String, nodeTpe: String): Unit = {
    // here we know which NODE is right-clicked
    // we save relevant ids, and then use them when delete is clicked
    hideAllMenus()
    this.nodeId = nodeId
    if DeleteableNodeTypes(nodeTpe) then
      val x = event.pageX
      val y = event.pageY
      nodeContextMenu.style.left = s"${x}px"
      nodeContextMenu.style.top = s"${y}px"
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

    // node buttons
    copyButton.addEventListener(
      "click",
      (_: dom.MouseEvent) =>
        val stmtJson = programModel.findStatement(nodeId).toJson
        dom.window.navigator.clipboard.writeText(stmtJson)
        flowrunChannel := FlowRun.Event.Deselected
    )

    deleteButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => programModel.delete(nodeId)
    )

    // edge buttons
    pasteButton.addEventListener(
      "click",
      (_: dom.MouseEvent) =>
        dom.window.navigator.clipboard.readText().`then` { copiedText =>
          try {
            val newStmt = copiedText.parseJson[Statement].duplicated
            addStatement(newStmt)
          } catch {
            case NonFatal(_) =>
              toastify.Toastify(ToastifyOptions("Not a valid statement", Color.yellow)).showToast()
          }
        }
    )

    addDeclareButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(Declare(AST.newId, "x", Expression.Type.Integer, None, 1))
    )

    addAssignButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(Assign(AST.newId, "x", "19"))
    )

    addInputButton.addEventListener(
      "click",
      (_: dom.MouseEvent) =>
        val prompt = Option.when(programModel.ast.config.useInputPrompt)("Please enter x:")
        addStatement(Input(AST.newId, "x", prompt))
    )

    addOutputButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(Output(AST.newId, "\"output\"", true))
    )

    addCallButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(Call(AST.newId, "fun1()"))
    )

    addIfButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(If(AST.newId, "true", Block(AST.newId), Block(AST.newId)))
    )

    addWhileButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(While(AST.newId, "false", Block(AST.newId)))
    )

    addDoWhileButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(DoWhile(AST.newId, "false", Block(AST.newId)))
    )

    addForLoopButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(ForLoop(AST.newId, "i", "0", "1", "10", Block(AST.newId)))
    )

    addCommentButton.addEventListener(
      "click",
      (_: dom.MouseEvent) => addStatement(Comment(AST.newId, "comment"))
    )
  }

  private def addStatement(stmt: Statement): Unit =
    programModel.addStmt(stmt, blockId, afterId)

}
