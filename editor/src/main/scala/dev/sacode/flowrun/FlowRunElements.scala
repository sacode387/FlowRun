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
  val programNameInput: dom.html.Input = mountElem.querySelector(".flowrun-program-name").asInstanceOf[dom.html.Input]

  val functionsList: dom.Element = mountElem.querySelector(".flowrun-functions-list")
  val functionsChooser: dom.Element = mountElem.querySelector(".flowrun-fun-chooser")

  val drawArea: dom.Element = mountElem.querySelector(".flowrun-draw")

  val configWidget: dom.Element = mountElem.querySelector(".flowrun-config")
  val debugVariables: dom.Element = mountElem.querySelector(".flowrun-debug-vars")

  val codeArea: dom.html.Element = mountElem.querySelector(".flowrun-code").asInstanceOf[dom.html.Element]
  val codeLang: dom.html.Select = mountElem.querySelector(".flowrun-gencode-lang").asInstanceOf[dom.html.Select]

  val output: dom.Element = mountElem.querySelector(".flowrun-output")
  val stmtOutput: dom.Element = mountElem.querySelector(".flowrun-output-statement")
  val syntaxOutput: dom.Element = mountElem.querySelector(".flowrun-output-syntax")
  val runtimeOutput: dom.Element = mountElem.querySelector(".flowrun-output-runtime")

  // singletons
  val runButton: dom.html.Element = mountElem.querySelector(".flowrun-btn-run").asInstanceOf[dom.html.Element]
  val runStepButton: dom.html.Element = mountElem.querySelector(".flowrun-btn-run-step").asInstanceOf[dom.html.Element]
  val stopButton: dom.html.Element = mountElem.querySelector(".flowrun-btn-stop").asInstanceOf[dom.html.Element]

  val flowrunDrawIoBtns: dom.html.Element =
    mountElem.querySelector(".flowrun-draw-io-btns").asInstanceOf[dom.html.Element]
  val downloadButton: dom.html.Element =
    mountElem.querySelector(".flowrun-btn-download").asInstanceOf[dom.html.Element]
  val loadButton: dom.html.Button =
    mountElem.querySelector(".flowrun-btn-load").asInstanceOf[dom.html.Button]

  val copySourceButton: dom.html.Element =
    mountElem.querySelector(".flowrun-btn-copy-source").asInstanceOf[dom.html.Element]
  val pasteSourceButton: dom.html.Button =
    mountElem.querySelector(".flowrun-btn-paste-source").asInstanceOf[dom.html.Button]

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

  val showFunctionsCheckbox: dom.html.Input =
    mountElem.querySelector(".flowrun-cb-show-functions").asInstanceOf[dom.html.Input]
  val showCodeCheckbox: dom.html.Input =
    mountElem.querySelector(".flowrun-cb-show-gen-code").asInstanceOf[dom.html.Input]
  val showDebugVarsCheckbox: dom.html.Input =
    mountElem.querySelector(".flowrun-cb-show-debug-vars").asInstanceOf[dom.html.Input]
  val showIoBtnsCheckbox: dom.html.Input =
    mountElem.querySelector(".flowrun-cb-show-io-btns").asInstanceOf[dom.html.Input]
  val useInputPromptCheckbox: dom.html.Input =
    mountElem.querySelector(".flowrun-cb-use-input-prompt").asInstanceOf[dom.html.Input]
  val echoEnteredValueCheckbox: dom.html.Input =
    mountElem.querySelector(".flowrun-cb-echo-entered-value").asInstanceOf[dom.html.Input]

  val showConfigButton =
    mountElem.querySelector(".flowrun-btn-config").asInstanceOf[dom.html.Element]
  val closeConfigButton =
    mountElem.querySelector(".flowrun-btn-config-close").asInstanceOf[dom.html.Element]
  val configDialog: dom.html.Element =
    mountElem.querySelector(".flowrun-config").asInstanceOf[dom.html.Element]
  val zoomResetButton =
    mountElem.querySelector(".flowrun-btn-zoom-reset").asInstanceOf[dom.html.Element]

  val clearOutputBtn = mountElem.querySelector(".flowrun-btn-clear-output").asInstanceOf[dom.html.Element]

  // general
  private val enterButton = mountElem.querySelector(".flowrun-btn-enter").asInstanceOf[dom.html.Element]
  private val inputText = mountElem.querySelector(".flowrun-input-text").asInstanceOf[dom.html.Input]
  private val inputSelect = mountElem.querySelector(".flowrun-input-select").asInstanceOf[dom.html.Select]
  private val inputRadio = mountElem.querySelector(".flowrun-input-radio").asInstanceOf[dom.html.Input]

  // clean up
  mountElem.querySelector(".flowrun-template-transient").remove()

  programNameInput.value = ""
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
