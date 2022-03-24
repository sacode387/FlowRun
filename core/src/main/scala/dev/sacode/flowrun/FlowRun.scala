package dev.sacode.flowrun

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import scalatags.JsDom.all.*
import org.getshaka.nativeconverter.fromJson
import reactify.*
import dev.sacode.flowrun.eval.Interpreter
import dev.sacode.flowrun.ast.*
import dev.sacode.flowrun.edit.FlowchartPresenter
import dev.sacode.flowrun.edit.FunctionSelector
import dev.sacode.flowrun.edit.StatementEditor
import dev.sacode.flowrun.edit.OutputArea
import dev.sacode.flowrun.edit.DebugArea
import dev.sacode.flowrun.edit.CtxMenu
import dev.sacode.flowrun.codegen.CodeGeneratorFactory
import dev.sacode.flowrun.codegen.Language
import dev.sacode.flowrun.toastify.*
import dev.sacode.flowrun.eval.Interpreter.State
import java.time.Instant
import java.time.Duration
import java.time.temporal.TemporalUnit
import java.time.temporal.ChronoUnit
import dev.sacode.flowrun.edit.CodeArea

@JSExportTopLevel("FlowRun")
class FlowRun(
    mountElem: dom.html.Element,
    colorScheme: ColorScheme = ColorScheme.default,
    editable: Boolean = true,
    programJson: String = null,
    mountCallback: js.Function1[FlowRun, Unit] = null,
    changeCallback: js.Function1[FlowRun, Unit] = null
) {

  // resolve initial program
  private val jsonSourceOpt = Option(programJson).filterNot(_.trim.isEmpty).orElse {
    val mountElemText = mountElem.innerText.trim
    Option.when(mountElemText.nonEmpty)(mountElemText)
  }
  private val program = jsonSourceOpt match
    case Some(json) => json.fromJson[Program]
    case None =>
      Program(
        AST.newId,
        "My Program",
        Function(
          "main", // don't touch!
          "main",
          statements = List(Statement.Begin(AST.newId), Statement.Return(AST.newId))
        ),
        List.empty
      )

  // duplicate a template,
  // move all its children to mountElem
  // https://stackoverflow.com/a/20910214/4496364
  private val template = dom.document.getElementById("flowrun-template").cloneNode(true).asInstanceOf[dom.html.Element]
  mountElem.innerText = ""
  while template.childNodes.length > 0 do mountElem.appendChild(template.childNodes.head)

  private val flowRunConfig = FlowRunConfig.resolve()

  private val flowrunChannel = Channel[FlowRun.Event]
  private val programModel = ProgramModel(program, flowrunChannel)
  private var interpreter = Interpreter(programModel, flowrunChannel)

  @JSExport
  val flowRunElements = FlowRunElements(mountElem) // needs to come after JSON resolving and template copying
  private val flowchartPresenter = FlowchartPresenter(programModel, flowRunElements, colorScheme, flowrunChannel)
  private var outputArea = OutputArea(interpreter, flowRunElements, flowrunChannel)
  private var codeArea = CodeArea(flowRunElements, programModel, flowRunConfig)
  codeArea.init()
  private var debugArea = DebugArea(interpreter, flowRunElements)

  private val functionSelector = FunctionSelector(editable, programModel, flowrunChannel, flowRunElements)
  private val statementEditor = StatementEditor(programModel, flowrunChannel, flowRunElements)
  private val ctxMenu = CtxMenu(programModel, flowRunElements, flowrunChannel)

  private var startedTime: Instant = _

  flowRunElements.metaData.innerText = program.name

  attachRunAndCopyListeners()

  if editable then
    ctxMenu.init()
    attachEditListeners()
  else flowRunElements.addFunButton.remove()

  functionSelector.loadFunctions()
  codeArea.render("")

  flowRunConfig.attach { cfg =>
    doOnChange()
  }

  @JSExport
  def config(): FlowRunConfig =
    flowRunConfig.get

  @JSExport
  def json(): String =
    programModel.ast.toJson

  @JSExport
  def funDOT(): String =
    flowchartPresenter.funDOT

  @JSExport
  def codeText(): String =
    codeArea.codeText()

  import FlowRun.Event.*
  flowrunChannel.attach {
    case EvalSuccess =>
      val finishedTime = Instant.now()
      val duration = Duration.between(startedTime, finishedTime)
      flowRunElements.runtimeOutput.appendChild(
        div(cls := "flowrun-output-help")(samp(s"[Finished at: ${DTF.format(finishedTime)}]")).render
      )
      flowRunElements.runtimeOutput.appendChild(
        div(cls := "flowrun-output-help")(samp(s"[Execution time: ${duration.toString.substring(2)}]")).render
      )
      debugArea.clear()
      flowchartPresenter.enable()
      functionSelector.enable()
      outputArea.finished()
    case SyntaxSuccess =>
      outputArea.clearSyntax()
      flowchartPresenter.loadCurrentFunction() // if function name updated
    case StmtUpdated(nodeId) =>
      outputArea.clearSyntax()
      flowchartPresenter.loadCurrentFunction()
      doOnChange()
      doOnModelChange()
    case StmtDeleted | StmtAdded =>
      programModel.currentStmtId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      flowchartPresenter.loadCurrentFunction()
      doOnChange()
      doOnModelChange()
    case SyntaxError(msg) =>
      outputArea.syntaxError(msg)
      flowchartPresenter.enable()
      functionSelector.enable()
      outputArea.finished()
    case EvalError(nodeId, msg, funId) =>
      programModel.currentFunctionId = funId
      flowchartPresenter.loadCurrentFunction().foreach { _ =>
        outputArea.runtimeError(msg, DTF.format(startedTime), DTF.format(Instant.now()))
        flowchartPresenter.highlightError(nodeId)
        flowchartPresenter.enable()
        functionSelector.enable()
        functionSelector.loadFunctions()
        outputArea.finished()
        debugArea.clear()
      }
    case EvalOutput(output, newline) =>
      outputArea.runtimeOutput(output, newline)
    case EvalInput(nodeId, name, prompt) =>
      outputArea.evalInput(nodeId, name, prompt)
    case SymbolTableUpdated =>
      debugArea.showVariables()
    case FunctionUpdated =>
      flowchartPresenter.loadCurrentFunction()
      functionSelector.loadFunctions()
      outputArea.clearSyntax()
      doOnChange()
      doOnModelChange()
    case FunctionSelected =>
      programModel.currentStmtId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      functionSelector.loadFunctions()
      flowchartPresenter.loadCurrentFunction()
      doOnModelChange()
      doOnChange()
    case StmtSelected =>
      doOnChange()
    case Deselected =>
      programModel.currentStmtId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      flowchartPresenter.clearSelected()
      doOnChange()
    case ConfigChanged =>
      doOnChange()
    case SvgMounted =>
      Option(mountCallback).foreach { cb => cb(this) }
  }
  flowrunChannel.attach { _ =>
    // on any event hide menus
    ctxMenu.hideAllMenus()
  }

  // trigger first time to get the ball rolling
  flowrunChannel := FlowRun.Event.SyntaxSuccess

  private def attachRunAndCopyListeners(): Unit = {
    flowRunElements.runButton.onclick = _ => {
      outputArea.clearAll()
      outputArea.running()
      flowchartPresenter.clearErrors()
      flowchartPresenter.clearSelected()
      flowrunChannel := FlowRun.Event.Deselected

      startedTime = Instant.now()
      flowRunElements.runtimeOutput.appendChild(
        div(cls := "flowrun-output-help")(samp(s"[Started at: ${DTF.format(startedTime)}]")).render
      )

      interpreter = Interpreter(programModel, flowrunChannel) // fresh SymTable etc
      outputArea = OutputArea(interpreter, flowRunElements, flowrunChannel)
      debugArea = DebugArea(interpreter, flowRunElements)

      interpreter.run()
      dom.window.setTimeout(
        () => {
          // check after delay if it's running, to avoid flicker
          if Set(State.RUNNING, State.PAUSED).contains(interpreter.state) then
            flowchartPresenter.disable()
            functionSelector.disable()
        },
        100
      )
    }

    flowRunElements.stopButton.onclick = _ => {
      interpreter.state = State.FINISHED_STOPPED
    }

    flowRunElements.copySourceButton.onclick = _ => {
      dom.window.navigator.clipboard.writeText(json())
      Toastify(ToastifyOptions("Copied program source to clipboard.", Color.green)).showToast()
    }

    flowRunElements.copyDotButton.onclick = _ => {
      dom.window.navigator.clipboard.writeText(flowchartPresenter.funDOT)
      Toastify(ToastifyOptions("Copied DOT to clipboard.", Color.green)).showToast()
    }
  }

  private def attachEditListeners(): Unit = {
    flowRunElements.addFunButton.onclick = _ => programModel.addFunction()

    flowRunElements.drawArea.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        event.preventDefault()
        DomUtils.getNearestSvgNode(event) match {
          case ("NODE", n) =>
            val idParts = n.id.split("#", -1)
            val nodeId = idParts(0)
            if !n.classList.contains("flowrun-not-selectable") then
              programModel.currentStmtId = Some(nodeId)
              outputArea.clearSyntax()
              flowchartPresenter.loadCurrentFunction() // to highlight new node..
              statementEditor.edit(nodeId)
              flowrunChannel := StmtSelected
          case _ =>
            flowrunChannel := FlowRun.Event.Deselected
        }
      }
    )

    flowRunElements.drawArea.addEventListener(
      "contextmenu",
      (event: dom.MouseEvent) => {
        event.preventDefault()
        DomUtils.getNearestSvgNode(event) match {
          case ("NODE", n) =>
            val idParts = n.id.split("#", -1)
            val nodeId = idParts(0)
            val tpe = idParts(1)
            ctxMenu.handleNodeRightClick(event, nodeId, tpe)
          case ("EDGE", n) =>
            ctxMenu.handleEdgeRightClick(event.clientX, event.clientY, n)
          case _ =>
            flowrunChannel := FlowRun.Event.Deselected
        }
      }
    )

    flowRunElements.drawArea.addEventListener(
      "dblclick",
      (event: dom.MouseEvent) => {
        Toastify(ToastifyOptions("Please right click on arrow to add more nodes. (Long press on touchscreen)"))
          .showToast()
      }
    )
  }

  private def doOnChange(): Unit =
    val id = programModel.currentStmtId.getOrElse("")
    codeArea.render(id)

  private def doOnModelChange(): Unit =
    Option(changeCallback).foreach(cb => cb(this))

}

object FlowRun:

  enum Event:
    case EvalSuccess
    case EvalError(nodeId: String, msg: String, funId: String)
    case EvalOutput(msg: String, newline: Boolean)
    case EvalInput(nodeId: String, name: String, prompt: Option[String])
    case SyntaxSuccess
    case StmtDeleted
    case StmtAdded
    case StmtUpdated(nodeId: String)
    case SyntaxError(msg: String)
    case SymbolTableUpdated
    case FunctionUpdated
    case FunctionSelected
    case StmtSelected
    case Deselected
    case ConfigChanged
    case SvgMounted
end FlowRun
