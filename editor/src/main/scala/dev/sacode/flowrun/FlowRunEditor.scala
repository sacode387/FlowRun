package dev.sacode.flowrun

import java.time.Instant
import java.time.Duration
import java.time.temporal.TemporalUnit
import java.time.temporal.ChronoUnit
import scala.compiletime.uninitialized
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom

import scalatags.JsDom.all.*
import reactify.*
import ba.sake.tupson.*

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
import dev.sacode.flowrun.eval.Interpreter
import dev.sacode.flowrun.eval.Interpreter.State
import dev.sacode.flowrun.eval.Interpreter.ExecMode
import dev.sacode.flowrun.edit.CodeArea

enum EditMode(val editable: Boolean):
  case ReadOnly extends EditMode(false)
  case Restricted extends EditMode(true)
  case Edit extends EditMode(true)

// dont use Option here !!!
@JSExportTopLevel("FlowRunEditor")
class FlowRunEditor(
    mountElem: dom.html.Element,
    colorScheme: ColorScheme = null,
    programJson: String = null,
    mountCallback: js.Function1[FlowRunEditor, Unit] = null,
    changeCallback: js.Function1[FlowRunEditor, Unit] = null
) {

  private val mode: EditMode =
    if mountElem.classList.contains("flowrun-readonly") then EditMode.ReadOnly
    else if mountElem.classList.contains("flowrun-restricted") then EditMode.Restricted
    else EditMode.Edit

  // resolve initial program
  private val jsonSourceOpt = Option(programJson).filterNot(_.trim.isEmpty).orElse {
    val mountElemText = mountElem.innerText.trim
    Option.when(mountElemText.nonEmpty)(mountElemText)
  }
  private val program = jsonSourceOpt match
    case Some(json) => json.parseJson[Program]
    case None =>
      Program(
        AST.newId,
        "New Program",
        FlowRunConfig("java", true, true),
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

  private val flowrunChannel = Channel[FlowRun.Event]
  private val programModel = ProgramModel(program, flowrunChannel)
  private var interpreter = newInterpeter

  private def newInterpeter = Interpreter(
    programModel,
    flowrunChannel,
    setInterval = (interval, body) => js.timers.setInterval(interval.toDouble)(body),
    clearInterval = (handle) => js.timers.clearInterval(handle.asInstanceOf[js.timers.SetIntervalHandle])
  )

  @JSExport
  val flowRunElements = FlowRunElements(mountElem) // needs to come after JSON resolving and template copying
  private val colorSchemeSelected = Option(colorScheme).getOrElse(ColorScheme.default)
  private val flowchartPresenter = FlowchartPresenter(programModel, flowRunElements, colorScheme, flowrunChannel)
  private var outputArea = OutputArea(interpreter, flowRunElements, flowrunChannel)
  private var codeArea = CodeArea(flowRunElements, programModel)
  codeArea.init()
  private var debugArea = DebugArea(interpreter, flowRunElements)

  private val functionSelector = FunctionSelector(mode, programModel, flowrunChannel, flowRunElements)
  private val statementEditor = StatementEditor(programModel, flowrunChannel, flowRunElements)
  private val ctxMenu = CtxMenu(programModel, flowRunElements, flowrunChannel)

  private var startedTime: Instant = uninitialized

  flowRunElements.programNameInput.value = program.name

  attachRunAndCopyListeners()

  // fixed layout is when you force layout with a class
  // meaning not-changeable with checkboxes
  private val fixedLayout = flowRunElements.mountElem.classList.exists(_.startsWith("flowrun-layout"))
  if fixedLayout then flowRunElements.configWidget.querySelector(".flowrun-config-layout").remove()
  else updateLayout()

  if mode.editable then
    ctxMenu.init()
    attachEditListeners()

  flowRunElements.drawArea.addEventListener(
    "click",
    (event: dom.MouseEvent) => {
      event.preventDefault()
      DomUtils.getNearestSvgNode(event) match {
        case ("NODE", n) =>
          val idParts = n.id.split("#", -1)
          val nodeId = idParts(0)
          if !n.classList.contains("flowrun-not-selectable") then
            programModel.currentSelectedStmtId = Some(nodeId)
            if mode.editable then
              outputArea.clearSyntax()
              flowchartPresenter.loadCurrentFunction() // to highlight new node..
              statementEditor.edit(nodeId)
            end if
            flowrunChannel := FlowRun.Event.StmtSelected
        case ("EDGE", n) =>
          if mode.editable then ctxMenu.handleEdgeRightClick(event, n)
        case _ =>
          flowrunChannel := FlowRun.Event.Deselected
      }
    }
  )

  flowRunElements.programNameInput.style.width =
    "" + Math.max(flowRunElements.programNameInput.value.length + 3, 10) + "ch";

  functionSelector.loadFunctions()
  codeArea.render("")

  @JSExport
  def name(): String =
    programModel.ast.name

  @JSExport
  def json(): String =
    programModel.ast.toJson

  @JSExport
  def funDOT(): String =
    flowchartPresenter.funDOT

  @JSExport
  def codeText(): String =
    codeArea.codeText()

  @JSExport
  def revision(): Int =
    programModel.ast.revision

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
    case EvalBeforeExecStatement =>
      flowchartPresenter.highlightExecuting(interpreter.nextExecStatementId)
    case EvalAfterExecStatement =>
      flowchartPresenter.highlightExecuting(interpreter.nextExecStatementId)
    case SyntaxSuccess =>
      outputArea.clearSyntax()
      flowchartPresenter.loadCurrentFunction() // if function name updated
    case StmtUpdated(nodeId) =>
      outputArea.clearSyntax()
      flowchartPresenter.loadCurrentFunction()
      doOnChange()
      doOnModelChange()
    case StmtDeleted | StmtAdded =>
      programModel.currentSelectedStmtId = None
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
      programModel.currentSelectedFunctionId = funId
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
    case EvalFunctionStarted =>
      programModel.currentSelectedFunctionId = interpreter.currentExecFunctionId.getOrElse(ProgramModel.MainFunId)
      functionSelector.loadFunctions()
      flowchartPresenter.loadCurrentFunction()
    case EvalFunctionFinished =>
      programModel.currentSelectedFunctionId = interpreter.currentExecFunctionId.getOrElse(ProgramModel.MainFunId)
      flowchartPresenter.highlightExecuting(interpreter.nextExecStatementId)
      functionSelector.loadFunctions()
      flowchartPresenter.loadCurrentFunction()
    case SymbolTableUpdated =>
      debugArea.showVariables()
    case FunctionUpdated =>
      flowchartPresenter.loadCurrentFunction()
      functionSelector.loadFunctions()
      outputArea.clearSyntax()
      doOnChange()
      doOnModelChange()
    case FunctionSelected =>
      programModel.currentSelectedStmtId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      functionSelector.loadFunctions()
      flowchartPresenter.loadCurrentFunction()
      doOnModelChange()
      doOnChange()
    case StmtSelected =>
      doOnChange()
    case Deselected =>
      programModel.currentSelectedStmtId = None
      outputArea.clearStmt()
      outputArea.clearSyntax()
      flowchartPresenter.clearSelected()
      doOnChange()
    case ConfigChanged =>
      doOnChange()
      doOnModelChange()
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

    def doRun(execMode: ExecMode) = {
      outputArea.clearAll()
      outputArea.running()
      flowchartPresenter.clearErrors()
      flowchartPresenter.clearSelected()
      flowrunChannel := FlowRun.Event.Deselected

      startedTime = Instant.now()
      flowRunElements.runtimeOutput.appendChild(
        div(cls := "flowrun-output-help")(samp(s"[Started at: ${DTF.format(startedTime)}]")).render
      )

      interpreter = newInterpeter // fresh SymTable etc
      outputArea = OutputArea(interpreter, flowRunElements, flowrunChannel)
      debugArea = DebugArea(interpreter, flowRunElements)

      interpreter.run(execMode)
      dom.window.setTimeout(
        () => {
          // check after delay if it's running, to avoid flicker
          if Set(State.RUNNING, State.WAITING_FOR_INPUT).contains(interpreter.state) then
            flowchartPresenter.disable(interpreter.execMode)
            functionSelector.disable()
        },
        100
      )
    }
    flowRunElements.runButton.onclick = _ => {
      if interpreter.isRunning then interpreter.execMode = ExecMode.NORMAL
      else doRun(ExecMode.NORMAL)
    }

    flowRunElements.runStepButton.onclick = _ => {
      if !interpreter.isRunning then doRun(ExecMode.STEP_BY_STEP)

      // ako je RUNNING ne dat mu da steppa / restarta
      if interpreter.isRunning then interpreter.stepNext = true
    }

    flowRunElements.stopButton.onclick = _ => {
      interpreter.state = State.FINISHED_STOPPED
    }

    /* DOWNLOAD / LOAD */
    flowRunElements.downloadButton.onclick = _ => {
      import js.JSConverters._
      val blobContent = Seq(json())
      val file = new dom.Blob(blobContent.toJSIterable)
      val url = dom.URL.createObjectURL(file)
      val downloadLink = a(href := url, download := s"${program.name}.flowrun").render
      dom.document.body.appendChild(downloadLink)
      downloadLink.click()
      dom.window.setTimeout(
        () => {
          dom.document.body.removeChild(downloadLink)
          dom.URL.revokeObjectURL(url)
        },
        0
      )
    }

    if mode == EditMode.Edit then {
      flowRunElements.loadButton.onclick = _ => {
        import js.JSConverters._

        val inputForFile = input(tpe := "file", accept := ".flowrun").render
        inputForFile.onchange = (event: dom.Event) => {
          val file = inputForFile.files.item(0)
          file.text().`then` { fileText =>
            try {
              val loadedProgram = fileText.parseJson[Program].copy(id = AST.newId)
              programModel.ast = loadedProgram
              flowRunElements.programNameInput.value = loadedProgram.name
              flowrunChannel := FlowRun.Event.FunctionSelected
            } catch { e =>
              toastify.Toastify(ToastifyOptions("Not a valid program", Color.yellow)).showToast()
            }
          }
        }
        inputForFile.click()
      }
    } else {
      flowRunElements.loadButton.remove()
    }

    /* COPY */
    if mode == EditMode.ReadOnly then flowRunElements.copySourceButton.remove()
    else
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

    /* PASTE */
    if mode == EditMode.Edit then {
      flowRunElements.pasteSourceButton.onclick = _ => {
        dom.window.navigator.clipboard.readText().`then` { copiedText =>
          try {
            val loadedProgram = copiedText.parseJson[Program].copy(id = AST.newId)
            programModel.ast = loadedProgram
            flowrunChannel := FlowRun.Event.FunctionSelected
          } catch { e =>
            toastify.Toastify(ToastifyOptions("Not a valid program", Color.yellow)).showToast()
          }
        }
      }
    } else {
      flowRunElements.pasteSourceButton.remove()
    }

    /* CONFIG DIALOG */
    flowRunElements.showConfigButton.onclick = _ => {
      flowRunElements.configDialog.asDyn.showModal()
    }
    flowRunElements.closeConfigButton.onclick = _ => {
      flowRunElements.configDialog.asDyn.close()
    }

    /* OUTPUT */
    flowRunElements.clearOutputBtn.onclick = _ => {
      outputArea.clearRuntime()
    }
  }

  private def attachEditListeners(): Unit = {

    flowRunElements.programNameInput.classList.remove("flowrun--disabled")
    flowRunElements.programNameInput.maxLength = 30
    flowRunElements.programNameInput.oninput = _ => {
      programModel.setName(flowRunElements.programNameInput.value.trim)
      flowRunElements.programNameInput.style.width =
        "" + Math.max(flowRunElements.programNameInput.value.length + 3, 10) + "ch";
    }

    if mode.editable then flowRunElements.addFunButton.onclick = _ => programModel.addFunction()
    else flowRunElements.addFunButton.remove()

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
            ctxMenu.handleEdgeRightClick(event, n)
          case _ =>
            flowrunChannel := FlowRun.Event.Deselected
        }
      }
    )

    flowRunElements.showFunctionsCheckbox.checked = programModel.ast.config.showFunctions
    flowRunElements.showCodeCheckbox.checked = programModel.ast.config.showGenCode
    flowRunElements.showDebugVarsCheckbox.checked = programModel.ast.config.showDebugVars
    flowRunElements.showIoBtnsCheckbox.checked = programModel.ast.config.showIoBtns

    if !fixedLayout then
      flowRunElements.showFunctionsCheckbox.oninput = _ => setLayout()
      flowRunElements.showCodeCheckbox.oninput = _ => setLayout()

    flowRunElements.showDebugVarsCheckbox.oninput = _ => setLayout()
    flowRunElements.showIoBtnsCheckbox.oninput = _ => setLayout()
  }

  private def doOnChange(): Unit =
    val id = programModel.currentSelectedStmtId.getOrElse("")
    codeArea.render(id)

  private def doOnModelChange(): Unit =
    programModel.incrRevision()
    Option(changeCallback).foreach(cb => cb(this))

  private def setLayout(): Unit = {
    val oldConfig = programModel.ast.config
    val newConfig = oldConfig.copy(
      showFunctions = flowRunElements.showFunctionsCheckbox.checked,
      showGenCode = flowRunElements.showCodeCheckbox.checked,
      showDebugVars = flowRunElements.showDebugVarsCheckbox.checked,
      showIoBtns = flowRunElements.showIoBtnsCheckbox.checked
    )
    programModel.setConfig(newConfig)
    updateLayout()
  }

  private def updateLayout(): Unit = {
    val flowrunMount = flowRunElements.mountElem
    flowrunMount.classList.remove("flowrun-layout-f-d-o")
    flowrunMount.classList.remove("flowrun-layout-d-o_c")
    flowrunMount.classList.remove("flowrun-layout-d-o")
    flowrunMount.classList.remove("flowrun-layout-d_o")

    val config = programModel.ast.config
    val newLayout = resolveLayout(config.showFunctions, config.showGenCode)
    if newLayout.nonEmpty then flowrunMount.classList.add(newLayout)

    if config.showDebugVars then flowRunElements.debugVariables.classList.remove("flowrun--hidden")
    else flowRunElements.debugVariables.classList.add("flowrun--hidden")

    if config.showIoBtns then flowRunElements.flowrunDrawIoBtns.classList.remove("flowrun--hidden")
    else flowRunElements.flowrunDrawIoBtns.classList.add("flowrun--hidden")
  }

  private def resolveLayout(showFunctions: Boolean, showCode: Boolean): String = {
    if showFunctions && showCode then ""
    else if showFunctions && !showCode then "flowrun-layout-f-d-o"
    else if !showFunctions && showCode then "flowrun-layout-d-o_c"
    else "flowrun-layout-d_o"
  }

}
