package ba.sake.flowrun

import java.util.UUID
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.getshaka.nativeconverter.NativeConverter
import scalatags.JsDom.all.*
import reactify.*
import ba.sake.flowrun.eval.*
import ba.sake.flowrun.parse.parseExpr

@JSExportTopLevel("FlowRun")
class FlowRun(mountElem: dom.Element, programJson: Option[String] = None) {

  private val mountElemText = mountElem.innerText.trim

  private val maybeTemplate = dom.document.getElementById("FlowRun-template")
  private val flowRunElements = if maybeTemplate == null then defaultFlowRunElements
    else getFlowRunElements(maybeTemplate)
  mountElem.innerHTML = ""
  mountElem.appendChild(flowRunElements.template)

  private val maybeJson = programJson.orElse(Option.when(mountElemText.nonEmpty)(mountElemText))
  private val program = maybeJson match
    case Some(json) => NativeConverter[Program].fromNative(js.JSON.parse(json))
    case None => Program(UUID.randomUUID.toString, "program", Function("main"), List.empty)

  private val flowrunChannel = Channel[FlowRun.Event]
  private val programModel = ProgramModel(program)
  private val functionEditor = FunctionEditor(programModel, flowrunChannel, flowRunElements)
  private var interpreter = Interpreter(programModel, flowrunChannel)

  private var lastRun: String = ""

  flowRunElements.metaData.innerText = program.name

  populateFunctions()

  def json(): js.Any =
    programModel.ast.toNative

  private def populateFunctions(): Unit =
    val allFunctions = List(programModel.ast.main) ++ programModel.ast.functions

    val functionSelector = flowRunElements.newInputSelect
    functionSelector.name = s"${program.name}-currentFunction"
    functionSelector.onchange = { (e: dom.Event) =>
      val selectedFunName = e.target.asInstanceOf[dom.html.Input].value
      programModel.currentFunctionName = selectedFunName
      functionEditor.loadCurrentFunction()
      populateFunctions()
    }
    allFunctions.foreach { f =>
      val maybeSelected = Option.when(f.name == programModel.currentFunctionName)(selected)
      val funItem = option(value := f.name, maybeSelected)(f.name).render
      functionSelector.add(funItem)
      funItem
    }

    val addFunButton = flowRunElements.addFunButton
    addFunButton.onclick = { (e: dom.Event) =>
      val newFunName = "fun123" // TODO parse longest, add 1..
      val newFun = Function(newFunName)
      programModel.addFunction(newFun)
      programModel.currentFunctionName = newFunName
      functionEditor.loadCurrentFunction()
      populateFunctions()
    }

    val deleteFunButton = flowRunElements.deleteFunButton
    deleteFunButton.onclick = { (e: dom.Event) =>
      programModel.deleteFunction(programModel.currentFunctionName)
      programModel.currentFunctionName = "main"
      functionEditor.loadCurrentFunction()
      populateFunctions()
    }

    val selectElem = frag(
      label("Function: "),
      functionSelector,
      Option.when(programModel.currentFunctionName != "main")(deleteFunButton),
      addFunButton
    )
    flowRunElements.functionsChooser.innerText = ""
    flowRunElements.functionsChooser.appendChild(selectElem.render)

  // run the program
  flowRunElements.runButton.onclick = _ => {
    functionEditor.clearErrors()
    lastRun = getNowTime
    flowRunElements.output.innerText = s"Started at: $lastRun"

    interpreter = Interpreter(programModel, flowrunChannel) // fresh SymTable etc
    interpreter.run()
  }

  import FlowRun.Event.*
  flowrunChannel.attach {
    case SyntaxError(msg) =>
      var output = s"Started at: $lastRun"
      output += "\nError: " + msg
      displayError(output)
    case EvalError(_, msg) =>
      var output = s"Started at: $lastRun"
      output += "\nError: " + msg
      displayError(output)
    case SyntaxSuccess =>
      flowRunElements.output.innerText = ""
      flowRunElements.output.classList.remove("error")
    case EvalOutput(output) =>
      val newOutput = pre(output).render
      flowRunElements.output.appendChild(newOutput)
    case EvalInput(nodeId, name) =>
      evalInput(nodeId, name)
    case SymbolTableUpdated =>
      showVariables()
  }

  private def evalInput(nodeId: String, name: String) = {
    
    val valueInputElem = input().render
    val valueBtnElem = button("Enter").render
    val enterValueDiv = div(
      label(
        s"Please enter value for '$name': ",
        valueInputElem,
        valueBtnElem
      )
    ).render
    flowRunElements.output.appendChild(enterValueDiv)

    valueInputElem.focus()

    valueBtnElem.onclick = _ => {
      val inputValue = valueInputElem.value.trim
      val key = SymbolKey(name, Symbol.Kind.Variable)
      val sym = interpreter.symTab.getSymbol(null, key)
      try {
        val value = sym.tpe.get match
          case Expression.Type.Integer  => inputValue.toInt
          case Expression.Type.Real     => inputValue.toDouble
          case Expression.Type.Boolean  => inputValue.toBoolean
          case Expression.Type.String   => inputValue
        interpreter.symTab.setValue(nodeId, name, value)
        interpreter.continue()

        val newOutput = pre(s"Please enter value for '$name': $inputValue").render
        flowRunElements.output.removeChild(enterValueDiv)
        flowRunElements.output.appendChild(newOutput)
      } catch {
        case (e: EvalException) => // from symbol table
          displayError(e.getMessage)
        case e: (NumberFormatException | IllegalArgumentException) =>
          displayError(s"Entered invalid ${sym.tpe.get}: '${inputValue}'")
      }
    }
  }

  private def displayError(msg: String): Unit =
    flowRunElements.output.innerText = msg
    flowRunElements.output.classList.add("error")

  private def showVariables(): Unit =
    flowRunElements.debugVariables.innerText = ""
    val varValues = interpreter.symTab.varSymbols
    varValues.foreach { sym =>
      val symElem = div(s"${sym.key.name}: ${sym.tpe.get} = ${sym.value.getOrElse("")}").render
      flowRunElements.debugVariables.appendChild(symElem)
    }
  
  private def getFlowRunElements(tmpl: dom.Element): FlowRunElements = {

    val template = tmpl.cloneNode(true).asInstanceOf[dom.Element]

    val addFunButton = template.querySelector(".FlowRun-add-function").cloneNode(true).asInstanceOf[dom.html.Element]
    val deleteFunButton = template.querySelector(".FlowRun-delete-function").cloneNode(true).asInstanceOf[dom.html.Element]

    val runButton = template.querySelector(".FlowRun-run").cloneNode(true).asInstanceOf[dom.html.Element]
    val inputText = template.querySelector(".FlowRun-input-text").cloneNode(true).asInstanceOf[dom.html.Input]
    val inputSelect = template.querySelector(".FlowRun-input-select").cloneNode(true).asInstanceOf[dom.html.Select]
    
    FlowRunElements(template,
      addFunButton, deleteFunButton,
      runButton, inputText, inputSelect)
  }

  private def defaultFlowRunElements: FlowRunElements = {
    val runButton = button("Run").render

    val addFunButton = button("Add").render
    val deleteFunButton = button("Delete").render

    val inputText = input(tpe := "text").render
    val inputSelect = select().render

    val template = div(
        div(cls := "FlowRun-meta")(),
        div(cls := "FlowRun-function")(),
        div(cls := "FlowRun-content")(
          div(cls := "FlowRun-draw", width := "100%", height := "100%")(),
          div(cls := "FlowRun-edit")(),
          div(cls := "FlowRun-output")(),
          div(cls := "FlowRun-debug")()
        )
      ).render
    FlowRunElements(template,
      addFunButton, deleteFunButton,
      runButton, inputText, inputSelect)
  }
}

object FlowRun:
  def parseJson(jsonString: String): Program =
    NativeConverter[Program].fromNative(js.JSON.parse(jsonString))

  enum Event:
    case SyntaxError(msg: String)
    case EvalError(nodeId: String, msg: String)
    case SyntaxSuccess
    case EvalOutput(msg: String)
    case EvalInput(nodeId: String, name: String)
    case SymbolTableUpdated

/* UI sections and elements */
case class FlowRunElements(
  template: dom.Element,
  // functions
  addFunButton: dom.html.Element,
  deleteFunButton: dom.html.Element,
  // other
  runButton: dom.html.Element,
  inputText: dom.html.Input,
  inputSelect: dom.html.Select
) {

  val metaData: dom.Element = template.querySelector(".FlowRun-meta")
  metaData.innerText = ""

  val functionsChooser: dom.Element = template.querySelector(".FlowRun-function") 
  functionsChooser.innerText = ""

  val drawArea: dom.Element = template.querySelector(".FlowRun-draw")
  drawArea.innerText = ""

  val editStatement: dom.Element = template.querySelector(".FlowRun-edit")
  editStatement.innerText = ""

  val outputDiv: dom.Element = template.querySelector(".FlowRun-output")
  val output = div().render
  outputDiv.innerText = ""
  outputDiv.appendChild(runButton)
  outputDiv.appendChild(output)

  val debugVariables: dom.Element = template.querySelector(".FlowRun-debug")
  debugVariables.innerText = ""

  def newInputText: dom.html.Input =
    inputText.cloneNode(true).asInstanceOf[dom.html.Input]
  
  def newInputSelect: dom.html.Select =
    inputSelect.cloneNode(true).asInstanceOf[dom.html.Select]
}

def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString
