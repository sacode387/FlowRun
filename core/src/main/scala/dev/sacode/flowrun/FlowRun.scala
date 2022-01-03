package dev.sacode.flowrun

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scalatags.JsDom.all.*
import org.getshaka.nativeconverter.NativeConverter
import reactify.*
import dev.sacode.flowrun.eval.Interpreter
import dev.sacode.flowrun.edit.FunctionEditor
import dev.sacode.flowrun.edit.FunctionSelector
import dev.sacode.flowrun.edit.StatementEditor
import dev.sacode.flowrun.edit.OutputArea
import dev.sacode.flowrun.edit.DebugArea
import dev.sacode.flowrun.edit.CtxMenu

@JSExportTopLevel("FlowRun")
class FlowRun(mountElem: dom.Element, programJson: Option[String] = None) {

  private val mountElemText = mountElem.innerText.trim

  private val maybeTemplate = dom.document.getElementById("flowrun-template").asInstanceOf[dom.html.Element]
  private val flowRunElements = FlowRunElements.resolve(maybeTemplate)
  mountElem.innerText = ""
  mountElem.appendChild(flowRunElements.metaData)
  mountElem.appendChild(flowRunElements.execBtns)
  mountElem.appendChild(flowRunElements.functionsChooser)
  mountElem.appendChild(flowRunElements.drawArea)
  mountElem.appendChild(flowRunElements.scratchpad)
  mountElem.appendChild(flowRunElements.debugVariables)

  private val maybeJson = programJson.orElse(
    Option.when(mountElemText.nonEmpty)(mountElemText)
  )
  private val program = maybeJson match
    case Some(json) => NativeConverter[Program].fromNative(js.JSON.parse(json))
    case None =>
      Program(
        AST.newId,
        "My Program",
        Function(
          "main",
          "main",
          statements = List(Statement.Begin(AST.newId), Statement.Return(AST.newId))
          /*statements = List(
          Statement.Begin(AST.newId),
          Statement.Declare(AST.newId, "x", Expression.Type.Integer, None),

          Statement.If(
            AST.newId,
            "true",
            Statement.Block(AST.newId, List(
              Statement.If(
                AST.newId,
                "true",
                Statement.Block(AST.newId, List(
                  Statement.Assign(AST.newId, "x", "2")
                )), // true
                Statement.Block(AST.newId, List(Statement.Assign(AST.newId, "x", "2"))) // false
              )
            )), // true
            Statement.Block(AST.newId, List(Statement.Assign(AST.newId, "x", "2"))) // false
          ),
          Statement.Return(AST.newId)
          )*/
        ),
        List.empty
      )

  private val flowrunChannel = Channel[FlowRun.Event]
  private val programModel = ProgramModel(program, flowrunChannel)
  private val functionEditor = FunctionEditor(programModel, flowrunChannel, flowRunElements)
  private val functionSelector = FunctionSelector(programModel, flowrunChannel, flowRunElements)
  private val statementEditor = StatementEditor(programModel, flowrunChannel, flowRunElements)
  private val ctxMenu = CtxMenu(flowRunElements, programModel)
  private var interpreter = Interpreter(programModel, flowrunChannel)
  private var outputArea = OutputArea(interpreter, flowRunElements)
  private var debugArea = DebugArea(interpreter, flowRunElements)

  private var startedTime: String = ""

  flowRunElements.metaData.innerText = program.name

  functionSelector.loadFunctions()

  /*
  dom.document.getElementById("gencode").asInstanceOf[dom.html.Button].onclick = _ => {
    val generator = new dev.sacode.flowrun.codegen.ScalaGenerator(programModel.ast)
    println(generator.generate)
  }
   */

  def json(): js.Any =
    programModel.ast.toNative

  // run the program
  flowRunElements.runButton.onclick = _ => {
    outputArea.clearAll()
    functionEditor.clearErrors()

    startedTime = getNowTime
    flowRunElements.runtimeOutput.appendChild(s"Started at: $startedTime".render)
    flowRunElements.runtimeOutput.classList.add("flowrun--success")
    flowRunElements.runtimeOutput.appendChild(br.render)
    flowRunElements.runtimeOutput.appendChild(br.render)

    interpreter = Interpreter(programModel, flowrunChannel) // fresh SymTable etc
    outputArea = OutputArea(interpreter, flowRunElements)
    debugArea = DebugArea(interpreter, flowRunElements)

    interpreter.run()
    functionEditor.disable()
  }

  flowRunElements.addFunButton.onclick = _ => programModel.addNewFunction()

  dom.window.addEventListener(
    "click",
    (event: dom.MouseEvent) => {
      event.preventDefault()
      getSvgNode(event.target) match {
        case ("NODE", n) =>
          val idParts = n.id.split("#")
          val nodeId = idParts(0)
          val tpe = idParts(1)
          statementEditor.edit(nodeId, tpe)
        case _ =>
          flowrunChannel := FlowRun.Event.Deselected
      }
    }
  )

  flowRunElements.drawArea.addEventListener(
    "contextmenu",
    (event: dom.MouseEvent) => {
      event.preventDefault()
      getSvgNode(event.target) match {
        case ("NODE", n) =>
          val idParts = n.id.split("#")
          val nodeId = idParts(0)
          val tpe = idParts(1)
          ctxMenu.handleRightClick(event, nodeId, tpe)
        case ("EDGE", n) =>
          ctxMenu.handleClick(event.clientX, event.clientY, n)
        case _ =>
      }
    }
  )

  import FlowRun.Event.*
  flowrunChannel.attach {
    case EvalSuccess =>
      flowRunElements.runtimeOutput.appendChild(br.render)
      flowRunElements.runtimeOutput.appendChild(br.render)
      flowRunElements.runtimeOutput.appendChild(s"Finished at: $getNowTime".render)
      functionEditor.enable()
    case SyntaxSuccess =>
      outputArea.clearSyntax()
      functionEditor.loadCurrentFunction() // if function name updated
    case SyntaxError(msg) =>
      outputArea.syntaxError(msg)
      functionEditor.enable()
    case EvalError(nodeId, msg) =>
      outputArea.runtimeError(msg, Some(startedTime), Some(getNowTime))
      functionEditor.highlightError(nodeId)
      functionEditor.enable()
    case EvalOutput(output) =>
      val newOutput = pre(output).render
      flowRunElements.runtimeOutput.appendChild(newOutput)
    case EvalInput(nodeId, name) =>
      outputArea.evalInput(nodeId, name)
    case SymbolTableUpdated =>
      debugArea.showVariables()
    case FunctionUpdated =>
      functionEditor.loadCurrentFunction()
      functionSelector.loadFunctions()
    case Deselected =>
      outputArea.clearStmt()
      outputArea.clearSyntax()
  }
  flowrunChannel.attach { _ =>
    // on any event hide menus
    ctxMenu.hideAllMenus()
  }
}

object FlowRun:

  enum Event:
    case EvalSuccess
    case EvalError(nodeId: String, msg: String)
    case EvalOutput(msg: String)
    case EvalInput(nodeId: String, name: String)
    case SyntaxSuccess
    case SyntaxError(msg: String)
    case SymbolTableUpdated
    case FunctionUpdated
    case Deselected
end FlowRun
