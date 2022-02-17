package dev.sacode.flowrun

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scalatags.JsDom.all.*
import org.getshaka.nativeconverter.fromJson
import reactify.*
import dev.sacode.flowrun.eval.Interpreter
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

@JSExportTopLevel("FlowRun")
class FlowRun(
    mountElem: dom.html.Element,
    editable: Boolean = true,
    programJson: Option[String] = None,
    mountCallback: Option[js.Function1[FlowRun, Unit]] = None,
    changeCallback: Option[js.Function1[FlowRun, Unit]] = None
) {

  // resolve initial program
  private val jsonSourceOpt = programJson.orElse {
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

  val flowRunElements = FlowRunElements(mountElem) // needs to come after JSON resolving and template copying
  private val flowchartPresenter = FlowchartPresenter(programModel, flowRunElements, flowrunChannel)
  private var outputArea = OutputArea(interpreter, flowRunElements, flowrunChannel)
  private var debugArea = DebugArea(interpreter, flowRunElements)

  private val functionSelector = FunctionSelector(editable, programModel, flowrunChannel, flowRunElements)
  private val statementEditor = StatementEditor(programModel, flowrunChannel, flowRunElements)
  private val ctxMenu = CtxMenu(programModel, flowRunElements)

  private var startedTime: Instant = _

  flowRunElements.metaData.innerText = program.name

  attachRunAndCopyListeners()

  if editable then
    ctxMenu.init()
    attachEditListeners()
  else flowRunElements.addFunButton.remove()

  functionSelector.loadFunctions()

  flowRunConfig.attach { _ =>
    flowrunChannel := FlowRun.Event.ConfigChanged
  }

  // trigger first time to get the ball rolling
  flowrunChannel := FlowRun.Event.SyntaxSuccess

  def config(): FlowRunConfig =
    flowRunConfig.get

  def json(): String =
    programModel.ast.toJson

  def funDOT(): String =
    flowchartPresenter.funDOT

  def codeText(): String =
    val generator = CodeGeneratorFactory(flowRunConfig.get.lang, programModel.ast)
    val codeTry = generator.generate
    if codeTry.isFailure then println("Failed to generate code: " + codeTry.failed)
    codeTry.getOrElse("Error while generating code:\n" + codeTry.failed.get.getMessage)

  import FlowRun.Event.*
  flowrunChannel.attach {
    case EvalSuccess =>
      val finishedTime = Instant.now()
      val duration = Duration.between(startedTime, finishedTime)
      flowRunElements.runtimeOutput.appendChild(div(br, samp(s"Finished at: ${DTF.format(finishedTime)}")).render)
      flowRunElements.runtimeOutput.appendChild(div(samp(s"Execution time: ${duration.toString.substring(2)}")).render)
      flowRunElements.debugVariables.innerText = ""
      flowchartPresenter.enable()
      functionSelector.enable()
      outputArea.finished()
    case SyntaxSuccess =>
      outputArea.clearSyntax()
      flowchartPresenter.loadCurrentFunction() // if function name updated
    case StmtDeleted | StmtAdded =>
      programModel.currentStmtId = None
      programModel.currentEdgeId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      flowchartPresenter.loadCurrentFunction()
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
        outputArea.finished()
      }
    case EvalOutput(output) =>
      val newOutput = div(samp(output), br).render
      flowRunElements.runtimeOutput.appendChild(newOutput)
    case EvalInput(nodeId, name) =>
      outputArea.evalInput(nodeId, name)
    case SymbolTableUpdated =>
      debugArea.showVariables()
    case FunctionUpdated =>
      flowchartPresenter.loadCurrentFunction()
      functionSelector.loadFunctions()
      outputArea.clearSyntax()
    case FunctionSelected =>
      programModel.currentStmtId = None
      programModel.currentEdgeId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      functionSelector.loadFunctions()
      flowchartPresenter.loadCurrentFunction()
    case Deselected =>
      programModel.currentStmtId = None
      programModel.currentEdgeId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      flowchartPresenter.clearSelected()
    case ConfigChanged => // noop
    case SvgMounted =>
      mountCallback.foreach { cb => cb(this) }
  }
  flowrunChannel.attach { _ =>
    // on any event hide menus
    ctxMenu.hideAllMenus()
    // gen code always
    generateCode()
  }

  private def attachRunAndCopyListeners(): Unit = {
    flowRunElements.runButton.onclick = _ => {
      outputArea.clearAll()
      outputArea.running()
      flowchartPresenter.clearErrors()
      flowchartPresenter.clearSelected()
      flowrunChannel := FlowRun.Event.Deselected

      startedTime = Instant.now()
      flowRunElements.runtimeOutput.appendChild(div(samp(s"Started at: ${DTF.format(startedTime)}"), br, br).render)

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

    flowRunElements.copyGencodeButton.onclick = _ => {
      dom.window.navigator.clipboard.writeText(codeText())
      Toastify(ToastifyOptions("Copied generated code to clipboard.", Color.green)).showToast()
    }
  }

  private def attachEditListeners(): Unit = {
    flowRunElements.addFunButton.onclick = _ => programModel.addNewFunction()

    flowRunElements.drawArea.addEventListener(
      "click",
      (event: dom.MouseEvent) => {
        event.preventDefault()
        DomUtils.getNearestSvgNode(event) match {
          case ("NODE", n) =>
            val idParts = n.id.split("#", -1)
            val nodeId = idParts(0)
            val tpe = idParts(1)
            if !n.classList.contains("flowrun-not-selectable") then
              programModel.currentStmtId = Some(nodeId)
              outputArea.clearSyntax()
              flowchartPresenter.loadCurrentFunction() // to highlight new node..
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
        DomUtils.getNearestSvgNode(event) match {
          case ("NODE", n) =>
            val idParts = n.id.split("#", -1)
            val nodeId = idParts(0)
            val tpe = idParts(1)
            ctxMenu.handleRightClick(event, nodeId, tpe)
          case ("EDGE", n) =>
            programModel.currentEdgeId = Some(n.id)
            ctxMenu.handleClick(event.clientX, event.clientY, n)
            programModel.currentEdgeId.foreach { id =>
              dom.window.document
                .querySelectorAll(s""" .edge[id*="$id"] """)
                .foreach(_.classList.add("flowrun--selected"))
            }
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

  private def generateCode(): Unit = {
    // gen code always
    changeCallback match
      case None     => flowRunElements.codeArea.innerText = codeText()
      case Some(cb) => cb(this)
  }

}

object FlowRun:

  enum Event:
    case EvalSuccess
    case EvalError(nodeId: String, msg: String, funId: String)
    case EvalOutput(msg: String)
    case EvalInput(nodeId: String, name: String)
    case SyntaxSuccess
    case StmtDeleted
    case StmtAdded
    case SyntaxError(msg: String)
    case SymbolTableUpdated
    case FunctionUpdated
    case FunctionSelected
    case Deselected
    case ConfigChanged
    case SvgMounted
end FlowRun
