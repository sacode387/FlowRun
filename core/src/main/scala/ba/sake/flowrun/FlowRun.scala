package ba.sake.flowrun

import java.util.UUID
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.getshaka.nativeconverter.NativeConverter
import scalatags.JsDom.all.*
import reactify.*
import ba.sake.flowrun.eval.*
import ba.sake.flowrun.edit.FunctionEditor
import ba.sake.flowrun.parse.parseExpr

@JSExportTopLevel("FlowRun")
class FlowRun(mountElem: dom.Element, programJson: Option[String] = None) {

  private val mountElemText = mountElem.innerText.trim

  private val maybeTemplate = dom.document.getElementById("FlowRun-template")
  private val flowRunElements = FlowRunElements.resolve(maybeTemplate)
  mountElem.innerHTML = ""
  mountElem.appendChild(flowRunElements.template)

  private val maybeJson = programJson.orElse(Option.when(mountElemText.nonEmpty)(mountElemText))
  private val program = maybeJson match
    case Some(json) => NativeConverter[Program].fromNative(js.JSON.parse(json))
    case None => Program(UUID.randomUUID.toString, "program", Function("main", "main"), List.empty)

  private val flowrunChannel = Channel[FlowRun.Event]
  private val programModel = ProgramModel(program)
  private val functionEditor = FunctionEditor(programModel, flowrunChannel, flowRunElements)
  private var interpreter = Interpreter(programModel, flowrunChannel)

  private var lastRun: String = ""

  flowRunElements.metaData.innerText = program.name

  populateFunctions()

  def json(): js.Any =
    programModel.ast.toNative
  
  def allFunctions = List(programModel.ast.main) ++ programModel.ast.functions

  private def populateFunctions(): Unit =

    val functionSelector = flowRunElements.newInputSelect
    functionSelector.name = s"${program.id}-currentFunction"
    functionSelector.onchange = { (e: dom.Event) =>
      val selectedFunId = e.target.asInstanceOf[dom.html.Input].value
      programModel.currentFunctionId = selectedFunId
      functionEditor.loadCurrentFunction()
      populateFunctions()
    }
    allFunctions.foreach { f =>
      val maybeSelected = Option.when(f.id == programModel.currentFunctionId)(selected)
      val funItem = option(value := f.id, maybeSelected)(f.name).render
      functionSelector.add(funItem)
    }

    

    val deleteFunButton = flowRunElements.deleteFunButton
    deleteFunButton.onclick = { (e: dom.Event) =>
      programModel.deleteFunction(programModel.currentFunctionId)
      programModel.currentFunctionId = "main"
      functionEditor.loadCurrentFunction()
      populateFunctions()
    }

    val selectElem = frag(
      label("Function: "),
      functionSelector,
      Option.unless(programModel.currentFunction.isMain)(deleteFunButton),
      flowRunElements.addFunButton
    )
    flowRunElements.functionsChooser.innerText = ""
    flowRunElements.functionsChooser.appendChild(selectElem.render)
  end populateFunctions

  // run the program
  flowRunElements.runButton.onclick = _ => {
    functionEditor.clearErrors()
    lastRun = getNowTime
    flowRunElements.output.innerText = ""
    flowRunElements.output.appendChild(s"Started at: $lastRun".render)

    interpreter = Interpreter(programModel, flowrunChannel) // fresh SymTable etc
    interpreter.run()
  }

  flowRunElements.addFunButton.onclick = { (e: dom.Event) =>
    val lastFunNum =  allFunctions.map(_.name).filter(_.startsWith("fun")).map(_.substring(3))
      .filter(_.toIntOption.isDefined)
      .map(_.toInt).maxOption.getOrElse(0)
    val newFunName = "fun" + (lastFunNum+1)
    val newFun = Function(UUID.randomUUID.toString, newFunName, List.empty, None,
      List(Statement.Return(UUID.randomUUID.toString))
    )
    programModel.addFunction(newFun)
    programModel.currentFunctionId = newFun.id
    functionEditor.loadCurrentFunction()
    populateFunctions()
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
    case FunctionUpdated =>
      populateFunctions()
  }

  private def evalInput(nodeId: String, name: String) = {
    
    val valueInputElem = flowRunElements.newInputText
    val valueBtnElem = flowRunElements.newEnterButton
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
    case FunctionUpdated


def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString
