package dev.sacode.flowrun

import scala.scalajs.js.annotation.*
import org.scalajs.dom
import scalatags.JsDom.all.*

/* UI sections and elements */
@JSExportAll
class FlowRunElements(
    val mountElem: dom.html.Element
) {

  // areas
  val metaData: dom.Element = mountElem.querySelector(".flowrun-meta")

  val functionsList: dom.Element = mountElem.querySelector(".flowrun-functions-list")
  val functionsChooser: dom.Element = mountElem.querySelector(".flowrun-fun-chooser")

  val drawArea: dom.Element = mountElem.querySelector(".flowrun-draw")

  val debugVariables: dom.Element = mountElem.querySelector(".flowrun-debug-vars")

  val codeArea: dom.html.Element = mountElem.querySelector(".flowrun-code").asInstanceOf[dom.html.Element]

  val output: dom.Element = mountElem.querySelector(".flowrun-output")
  val stmtOutput: dom.Element = mountElem.querySelector(".flowrun-output-statement")
  val syntaxOutput: dom.Element = mountElem.querySelector(".flowrun-output-syntax")
  val runtimeOutput: dom.Element = mountElem.querySelector(".flowrun-output-runtime")

  // singletons
  val runButton: dom.html.Element = mountElem.querySelector(".flowrun-btn-run").asInstanceOf[dom.html.Element]
  val stopButton: dom.html.Element = mountElem.querySelector(".flowrun-btn-stop").asInstanceOf[dom.html.Element]
  val copySourceButton: dom.html.Element =
    mountElem.querySelector(".flowrun-btn-copy-source").asInstanceOf[dom.html.Element]
  val copyDotButton: dom.html.Element = mountElem.querySelector(".flowrun-btn-copy-dot").asInstanceOf[dom.html.Element]
  val copyGencodeButton: dom.html.Element =
    mountElem.querySelector(".flowrun-btn-copy-gencode").asInstanceOf[dom.html.Element]

  val addFunButton: dom.html.Element = mountElem.querySelector(".flowrun-btn-fun-add").asInstanceOf[dom.html.Element]
  private val deleteFunButton: dom.html.Element =
    mountElem.querySelector(".flowrun-btn-fun-delete").asInstanceOf[dom.html.Element]

  val addParamButton: dom.html.Element =
    mountElem.querySelector(".flowrun-btn-fun-add-param").asInstanceOf[dom.html.Element]
  val deleteParamButton: dom.html.Element =
    mountElem.querySelector(".flowrun-btn-fun-delete-param").asInstanceOf[dom.html.Element]

  // general
  private val enterButton = mountElem.querySelector(".flowrun-btn-enter").asInstanceOf[dom.html.Element]
  private val inputText = mountElem.querySelector(".flowrun-input-text").asInstanceOf[dom.html.Input]
  private val inputSelect = mountElem.querySelector(".flowrun-input-select").asInstanceOf[dom.html.Select]
  private val inputRadio = mountElem.querySelector(".flowrun-input-radio").asInstanceOf[dom.html.Input]

  // clean up
  mountElem.querySelector(".flowrun-template-transient").remove()

  metaData.innerText = ""
  functionsChooser.innerText = ""
  drawArea.innerText = ""
  debugVariables.innerText = ""

  def newInputText(size: Int = 10): dom.html.Input =
    val res = inputText.cloneNode(true).asInstanceOf[dom.html.Input]
    res.size = size
    res

  def newInputSelect: dom.html.Select =
    inputSelect.cloneNode(true).asInstanceOf[dom.html.Select]

  def newInputRadio: dom.html.Input =
    inputRadio.cloneNode(true).asInstanceOf[dom.html.Input]

  def newEnterButton: dom.html.Element =
    enterButton.cloneNode(true).asInstanceOf[dom.html.Element]

  def newDeleteFunButton: dom.html.Element =
    deleteFunButton.cloneNode(true).asInstanceOf[dom.html.Button]

  def newDeleteParamButton: dom.html.Element =
    deleteParamButton.cloneNode(true).asInstanceOf[dom.html.Button]

}
