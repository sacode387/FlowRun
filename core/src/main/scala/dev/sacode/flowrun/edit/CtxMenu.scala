package dev.sacode.flowrun.edit

import org.scalajs.dom
import dev.sacode.flowrun.FlowRunElements
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.ProgramModel.Request
import dev.sacode.flowrun.Expression.Type
import dev.sacode.flowrun.AST
import dev.sacode.flowrun.edit.FunctionEditor

class CtxMenu(
    flowRunElements: FlowRunElements,
    programModel: ProgramModel,
    functionEditor: FunctionEditor
) {

  /** used for delete */
  private var nodeId = ""

  private var afterId = ""
  private var blockId = ""

  def init(): Unit = {
    val edgeContextMenu =
      dom.document.getElementById("flowrun-edge-context-menu").asInstanceOf[dom.html.Element]
    val addDeclareButton =
      edgeContextMenu.querySelector("#flowrun-add-declare").asInstanceOf[dom.html.Element]
    val addAssignButton =
      edgeContextMenu.querySelector("#flowrun-add-assign").asInstanceOf[dom.html.Element]
    val addInputButton =
      edgeContextMenu.querySelector("#flowrun-add-input").asInstanceOf[dom.html.Element]
    val addOutputButton =
      edgeContextMenu.querySelector("#flowrun-add-output").asInstanceOf[dom.html.Element]
    val addCallButton =
      edgeContextMenu.querySelector("#flowrun-add-call").asInstanceOf[dom.html.Element]
    val addIfButton =
      edgeContextMenu.querySelector("#flowrun-add-if").asInstanceOf[dom.html.Element]
    val addWhileButton =
      edgeContextMenu.querySelector("#flowrun-add-while").asInstanceOf[dom.html.Element]

    val nodeContextMenu =
      dom.document.getElementById("flowrun-node-context-menu").asInstanceOf[dom.html.Element]
    val deleteButton =
      nodeContextMenu.querySelector("#flowrun-delete").asInstanceOf[dom.html.Element]

    flowRunElements.drawArea.addEventListener(
      "contextmenu",
      (event: dom.MouseEvent) => {

        // here we know which NODE/EDGE is right-clicked
        // we save relevant ids, and then use them when a button is clicked

        event.preventDefault()
        hideAllMenus()
        event.target match {
          case g: dom.svg.Element =>
            // click usually refers to a <title>, <path>, <polygon> etc
            // but the id is on the parent group <g> element
            g.parentNode match {
              case parent: dom.svg.G =>
                if parent.className.baseVal == "node" then
                  nodeContextMenu.style.left = s"${event.clientX}px"
                  nodeContextMenu.style.top = s"${event.clientY}px"
                  nodeContextMenu.classList.add("active")
                  nodeId = parent.id
                else if parent.className.baseVal == "edge" then
                  edgeContextMenu.style.left = s"${event.clientX}px"
                  edgeContextMenu.style.top = s"${event.clientY}px"
                  edgeContextMenu.classList.add("active")

                  val titleText = parent.getElementsByTagName("title")(0).textContent
                  setEdgeIds(titleText, parent.id)

                  println("TITLE: " + titleText)
                else println("noooooooo idea")
              case _ =>
                println("Not a group")
            }
          case _ =>
            println("Not an svg element")
        }
      }
    )

    // close menu when clicked anywhere
    dom.window.addEventListener("click", event => hideAllMenus())

    deleteButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        programModel.delete(Request.Delete(nodeId))
        functionEditor.loadCurrentFunction()
      }
    )

    addDeclareButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        programModel.addDeclare(
          Request.AddDeclare(AST.newId, "x", Type.Integer, afterId, blockId)
        )
        functionEditor.loadCurrentFunction()
      }
    )

    addAssignButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        programModel.addAssign(Request.AddAssign(AST.newId, afterId, blockId))
        functionEditor.loadCurrentFunction()
      }
    )

    addInputButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        programModel.addInput(Request.AddInput(AST.newId, afterId, blockId))
        functionEditor.loadCurrentFunction()
      }
    )

    addOutputButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        println(s"ADD OUT $afterId, $blockId")
        programModel.addOutput(Request.AddOutput(AST.newId, afterId, blockId))
        functionEditor.loadCurrentFunction()
      }
    )

    addCallButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        programModel.addCall(Request.AddCall(AST.newId, afterId, blockId))
        functionEditor.loadCurrentFunction()
      }
    )

    addIfButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        programModel.addIf(
          Request.AddIf(AST.newId, AST.newId, AST.newId, afterId, blockId)
        )
        functionEditor.loadCurrentFunction()
      }
    )

    addWhileButton.addEventListener(
      "click",
      (event: dom.MouseEvent) => {

        programModel.addOutput(Request.AddOutput(AST.newId, afterId, blockId))
        functionEditor.loadCurrentFunction()
      }
    )
  }

  private def hideAllMenus(): Unit =
    dom.document
      .getElementsByClassName("flowrun-context-menu")
      .foreach { e =>
        e.asInstanceOf[dom.html.Element].classList.remove("active")
      }

  private def setEdgeIds(title: String, id: String): Unit =
    val newAfterId = title.takeWhile(_ != ':')
    if newAfterId.startsWith("end_") then
      blockId = ""
      afterId = newAfterId.drop("end_".length)
    else
      blockId = id
      afterId = newAfterId

}
