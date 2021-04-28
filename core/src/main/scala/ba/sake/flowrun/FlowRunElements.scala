package ba.sake.flowrun

import scala.scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.*


/* UI sections and elements */
case class FlowRunElements(
  template: dom.Element,
  // functions
  addFunButton: dom.html.Element,
  deleteFunButton: dom.html.Element,
  // other
  enterButton: dom.html.Element,
  inputText: dom.html.Input,
  inputSelect: dom.html.Select
) {

  val metaData: dom.Element = template.querySelector(".FlowRun-meta")
  val functionsChooser: dom.Element = template.querySelector(".FlowRun-function")
  val runButton = template.querySelector(".FlowRun-btn-run").asInstanceOf[dom.html.Element]
  val drawArea: dom.Element = template.querySelector(".FlowRun-draw")
  val editStatement: dom.Element = template.querySelector(".FlowRun-edit")
  val output: dom.Element = template.querySelector(".FlowRun-output")
  val debugVariables: dom.Element = template.querySelector(".FlowRun-debug")

  metaData.innerText = ""
  functionsChooser.innerText = ""  
  drawArea.innerText = ""
  editStatement.innerText = ""
  output.innerText = ""
  debugVariables.innerText = ""

  def newInputText: dom.html.Input =
    inputText.cloneNode(true).asInstanceOf[dom.html.Input]
  
  def newInputSelect: dom.html.Select =
    inputSelect.cloneNode(true).asInstanceOf[dom.html.Select]
  
  def newEnterButton: dom.html.Element =
    enterButton.cloneNode(true).asInstanceOf[dom.html.Element]
}

object FlowRunElements {

  def resolve(maybeTemplate: dom.Element): FlowRunElements = {
    if maybeTemplate == null then defaultFlowRunElements
    else getFlowRunElements(maybeTemplate)
  }

  private def getFlowRunElements(tmpl: dom.Element): FlowRunElements = {

    val template = tmpl.cloneNode(true).asInstanceOf[dom.html.Element]
    template.id = ""
    template.style = "display: block;"

    val addFunButton = template.querySelector(".FlowRun-add-function").cloneNode(true).asInstanceOf[dom.html.Element]
    val deleteFunButton = template.querySelector(".FlowRun-delete-function").cloneNode(true).asInstanceOf[dom.html.Element]

    val enterButton = template.querySelector(".FlowRun-btn-enter").cloneNode(true).asInstanceOf[dom.html.Element]
    val inputText = template.querySelector(".FlowRun-input-text").cloneNode(true).asInstanceOf[dom.html.Input]
    val inputSelect = template.querySelector(".FlowRun-input-select").cloneNode(true).asInstanceOf[dom.html.Select]
    
    FlowRunElements(template,
      addFunButton, deleteFunButton,
      enterButton, inputText, inputSelect)
  }

  private def defaultFlowRunElements: FlowRunElements = {

    val addFunButton = button("Add").render
    val deleteFunButton = button("Delete").render

    val runButton = button("Run").render
    val enterButton = button("Run").render
    val inputText = input(tpe := "text").render
    val inputSelect = select().render

    val template = div(
        div(cls := "FlowRun-meta")(),
        div(cls := "FlowRun-function")(),
        div(cls := "FlowRun-btn-run")(),
        div(cls := "FlowRun-content")(
          div(cls := "FlowRun-draw", width := "100%", height := "100%")(),
          div(cls := "FlowRun-edit")(),
          div(cls := "FlowRun-output")(),
          div(cls := "FlowRun-debug")()
        )
      ).render
    FlowRunElements(template,
      addFunButton, deleteFunButton,
      enterButton, inputText, inputSelect)
  }
}