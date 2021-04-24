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
  mountElem.appendChild(flowRunElements.content)

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
    flowRunElements.addFunButton.onclick = { (e: dom.Event) =>
      val newFunName = "fun123"
      val newFun = Function(newFunName)
      programModel.addFunction(newFun)
      programModel.currentFunctionName = newFunName
      functionEditor.loadCurrentFunction()
      populateFunctions()
    }
    val selectElem = div(
      b("Functions"),
      allFunctions.map { f =>
        val funItem = flowRunElements.functionItem.cloneNode(true).asInstanceOf[dom.html.Element]
        val funRadio = funItem.querySelector("input").asInstanceOf[dom.html.Input]
        funRadio.name = s"${program.name}-currentFunction"
        funRadio.value = f.name
        funRadio.checked = f.name == programModel.currentFunctionName
        funRadio.onchange = { (e: dom.Event) =>
          val selectedFunName = e.target.asInstanceOf[dom.html.Input].value
          programModel.currentFunctionName = selectedFunName
          functionEditor.loadCurrentFunction()
        }
        val funLabel = funItem.querySelector("span")
        funLabel.innerText = f.name
        val funDeleteBtn = funItem.querySelector("button").asInstanceOf[dom.html.Element]
        funDeleteBtn.onclick = { (e: dom.Event) =>
          programModel.deleteFunction(f.name)
          programModel.currentFunctionName = "main"
          functionEditor.loadCurrentFunction()
          populateFunctions()
        }
        if f.name == "main" then funItem.removeChild(funDeleteBtn)
        funItem
      },
      flowRunElements.addFunButton
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
  
  private def getFlowRunElements(template: dom.Element): FlowRunElements = {
    val metaData = label().render
    val drawArea = div(width := "100%", height := "100%").render
    val editStatement = div().render
    val output = div().render
    val debugVariables = div().render

    val addFunButton = template.querySelector(".FlowRun-add-function").cloneNode(true).asInstanceOf[dom.html.Element]
    val functionItem = template.querySelector(".FlowRun-function-item").cloneNode(true).asInstanceOf[dom.Element]
    val functionsChooser = div().render

    val runButton = template.querySelector(".FlowRun-run").cloneNode(true).asInstanceOf[dom.html.Element]
    val inputText = template.querySelector(".FlowRun-input-text").cloneNode(true).asInstanceOf[dom.html.Input]
    
    FlowRunElements(metaData, drawArea, editStatement, output, debugVariables,
      addFunButton, functionsChooser, functionItem,
      runButton, inputText, Some(template))
  }

  private def defaultFlowRunElements: FlowRunElements = {
    val metaData = label().render
    val drawArea = div(width := "100%", height := "100%").render
    val editStatement = div().render
    val runButton = button("Run").render
    val addFunButton = button("Add").render
    val functionsChooser = div().render
    val functionItem = label(display := "block")(
      input(tpe := "radio"),
      span(), // label goes here
      button("Delete")
    ).render
    val output = div().render
    val debugVariables = div().render
    val inputText = input(tpe := "text").render
    FlowRunElements(metaData, drawArea, editStatement, output, debugVariables,
      addFunButton, functionsChooser, functionItem,
      runButton, inputText, None)
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

case class FlowRunElements(
  // sections
  metaData: dom.Element,
  drawArea: dom.Element,
  editStatement: dom.Element,
  output: dom.Element,
  debugVariables: dom.Element,
  // functions
  addFunButton: dom.html.Element,
  functionsChooser: dom.Element,
  functionItem: dom.Element,
  // other
  runButton: dom.html.Element,
  inputText: dom.html.Input,
  maybeTemplate: Option[dom.Element]
) {

  def newInputText: dom.html.Input =
    inputText.cloneNode(true).asInstanceOf[dom.html.Input]

  def content: dom.Node = maybeTemplate match
    case None => frag(
        div(cls := "FlowRun-meta")(metaData),
        div(cls := "FlowRun-content")(
          div(cls := "FlowRun-draw")(drawArea),
          div(cls := "FlowRun-edit")(functionsChooser, hr, editStatement),
          div(cls := "FlowRun-output")(runButton, output),
          div(cls := "FlowRun-debug")(debugVariables)
        )
      ).render
    case Some(tmpl) =>
      val template = tmpl.cloneNode(true).asInstanceOf[dom.Element]
      template.querySelector(".FlowRun-meta").innerText = ""  
      template.querySelector(".FlowRun-draw").innerText = ""  
      template.querySelector(".FlowRun-edit").innerText = ""  
      template.querySelector(".FlowRun-output").innerText = ""  
      template.querySelector(".FlowRun-debug").innerText = ""  
      template.querySelector(".FlowRun-meta").appendChild(metaData)
      template.querySelector(".FlowRun-draw").appendChild(drawArea)
      template.querySelector(".FlowRun-edit").appendChild(functionsChooser)
      template.querySelector(".FlowRun-edit").appendChild(editStatement)
      template.querySelector(".FlowRun-output").appendChild(runButton)
      template.querySelector(".FlowRun-output").appendChild(output)
      template.querySelector(".FlowRun-debug").appendChild(debugVariables)
      template
}

def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString
