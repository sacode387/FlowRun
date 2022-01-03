package dev.sacode.flowrun

import scala.scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.*

/* UI sections and elements */
case class FlowRunElements(
    template: dom.Element,
    // functions
    addFunButton: dom.html.Element,
    deleteFunButton: dom.html.Element,
    addParamButton: dom.html.Element,
    deleteParamButton: dom.html.Element,
    // other
    enterButton: dom.html.Element,
    inputText: dom.html.Input,
    inputSelect: dom.html.Select
) {

  val metaData: dom.Element = template.querySelector(".flowrun-meta")
  val functionsChooser: dom.Element = template.querySelector(".flowrun-fun-chooser")
  val runButton = template.querySelector(".flowrun-btn-run").asInstanceOf[dom.html.Element]
  val drawArea: dom.Element = template.querySelector(".flowrun-draw")
  val scratchpad: dom.Element = template.querySelector(".flowrun-scratchpad")
  val debugVariables: dom.Element = template.querySelector(".flowrun-debug")

  template.querySelector(".flowrun-template-transient").remove()

  metaData.innerText = ""
  functionsChooser.innerText = ""
  drawArea.innerText = ""
  scratchpad.innerText = ""
  debugVariables.innerText = ""

  def newInputText: dom.html.Input =
    inputText.cloneNode(true).asInstanceOf[dom.html.Input]

  def newInputSelect: dom.html.Select =
    inputSelect.cloneNode(true).asInstanceOf[dom.html.Select]

  def newEnterButton: dom.html.Element =
    enterButton.cloneNode(true).asInstanceOf[dom.html.Element]

  def newDeleteParamButton: dom.html.Element =
    deleteParamButton.cloneNode(true).asInstanceOf[dom.html.Element]

}

object FlowRunElements {

  def resolve(maybeTemplate: dom.Element): FlowRunElements = {
    if maybeTemplate == null then defaultFlowRunElements
    else getFlowRunElements(maybeTemplate)
  }

  private def getFlowRunElements(tmpl: dom.Element): FlowRunElements = {

    val template = tmpl.cloneNode(true).asInstanceOf[dom.html.Element]
    template.id = ""
    template.style = "" // "display: block;"

    val addFunButton =
      template.querySelector(".flowrun-fun-add").cloneNode(true).asInstanceOf[dom.html.Element]
    val deleteFunButton = template
      .querySelector(".flowrun-fun-delete")
      .cloneNode(true)
      .asInstanceOf[dom.html.Element]

    val addParamButton =
      template.querySelector(".flowrun-fun-add-param").cloneNode(true).asInstanceOf[dom.html.Element]
    val deleteParamButton =
      template.querySelector(".flowrun-fun-delete-param").cloneNode(true).asInstanceOf[dom.html.Element]

    val enterButton =
      template.querySelector(".flowrun-btn-enter").cloneNode(true).asInstanceOf[dom.html.Element]
    val inputText =
      template.querySelector(".flowrun-input-text").cloneNode(true).asInstanceOf[dom.html.Input]
    val inputSelect =
      template.querySelector(".flowrun-input-select").cloneNode(true).asInstanceOf[dom.html.Select]

    FlowRunElements(
      template,
      addFunButton,
      deleteFunButton,
      addParamButton,
      deleteParamButton,
      enterButton,
      inputText,
      inputSelect
    )
  }

  private def defaultFlowRunElements: FlowRunElements = {

    val addFunButton = button("Add").render
    val deleteFunButton = button("Delete").render

    val addParamButton = button("+").render
    val deleteParamButton = button("-").render

    val runButton = button("Run").render
    val enterButton = button("Run").render
    val inputText = input(tpe := "text").render
    val inputSelect = select().render

    val template = div(
      div(cls := "flowrun-meta")(),
      div(cls := "flowrun-function")(),
      div(cls := "flowrun-btn-run")(),
      div(cls := "flowrun-content")(
        div(cls := "flowrun-draw")(),
        div(cls := "flowrun-edit")(),
        div(cls := "flowrun-output")(),
        div(cls := "flowrun-debug")()
      )
    ).render
    FlowRunElements(
      template,
      addFunButton,
      deleteFunButton,
      addParamButton,
      deleteParamButton,
      enterButton,
      inputText,
      inputSelect
    )
  }
}
