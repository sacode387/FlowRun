package ba.sake.flowrun

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.getshaka.nativeconverter.NativeConverter
import scalatags.JsDom.all._
import reactify._
import ba.sake.flowrun.cytoscape.CytoscapeFlowchart
import ba.sake.flowrun.eval._
import ba.sake.flowrun.parse.parseExpr

@JSExportTopLevel("FlowRun")
class FlowRun(program: Program, flowRunElements: FlowRunElements) {

  private val flowrunChannel = Channel[FlowRun.Event]
  private val programModel = ProgramModel(program)
  private val cytoscapeFlowchart = CytoscapeFlowchart(programModel, flowrunChannel, flowRunElements.drawArea, flowRunElements.editStatement)
  private var interpreter = Interpreter(programModel, flowrunChannel)

  private var lastRun: String = ""

  populateFunctions()

  def json(): js.Any =
    programModel.ast.toNative

  private def populateFunctions(): Unit =
    val allFunctions = List(programModel.ast.main) ++ programModel.ast.functions
    val selectElem = div(
      allFunctions.map { f =>
        val maybeSelected = Option.when(f.name == programModel.currentFunctionName)(checked)
        val maybeDelete = Option.when(f.name != "main") {
          button("Delete", onclick := { (e: dom.Event) =>
            programModel.deleteFunction(f.name)
            programModel.currentFunctionName = "main"
            cytoscapeFlowchart.loadCurrentFunction()
            populateFunctions()
          })
        }
        div(
          label(
            input(
              tpe := "radio", name := "currentFunction", value := f.name, maybeSelected,
              onchange := { (e: dom.Event) =>
                val selectedFunName = e.target.asInstanceOf[dom.html.Input].value
                programModel.currentFunctionName = selectedFunName
                cytoscapeFlowchart.loadCurrentFunction()
              }
            ),
            f.name,
            maybeDelete
          )
        )
      },
      button("Add", onclick := { (e: dom.Event) =>
        val newFunName = "fun123"
        val newFun = Function(newFunName)
        programModel.addFunction(newFun)
        programModel.currentFunctionName = newFunName
        cytoscapeFlowchart.loadCurrentFunction()
        populateFunctions()
      })
    )
    flowRunElements.functionsChooser.innerText = ""
    flowRunElements.functionsChooser.appendChild(selectElem.render)

  // run the program
  flowRunElements.runButton.asInstanceOf[dom.html.Button].onclick = _ => {
    cytoscapeFlowchart.clearErrors()
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
  drawArea: dom.Element,
  editStatement: dom.Element,
  runButton: dom.Element,
  functionsChooser: dom.Element,
  output: dom.Element,
  debugVariables: dom.Element
)

def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString
