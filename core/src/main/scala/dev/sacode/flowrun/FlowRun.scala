package dev.sacode.flowrun

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scalatags.JsDom.all.*
import org.getshaka.nativeconverter.NativeConverter
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

@JSExportTopLevel("FlowRun")
class FlowRun(
    mountElem: dom.Element,
    programJson: Option[String] = None,
    changeCallback: Option[js.Function1[FlowRun, Unit]] = None
) {

  private val FlowRunConfigKey = "flowrun-config"
  private val localConfig = initLocalConfig()
  def config(): FlowRunConfig = localConfig.get

  private val mountElemText = mountElem.innerText.trim

  private val maybeTemplate = dom.document.getElementById("flowrun-template").asInstanceOf[dom.html.Element]

  val flowRunElements = FlowRunElements.resolve(maybeTemplate)
  mountElem.innerText = ""
  // move all template children to mountElem
  // https://stackoverflow.com/a/20910214/4496364
  while flowRunElements.template.childNodes.length > 0 do
    mountElem.appendChild(flowRunElements.template.childNodes.head)

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
          "main", // don't touch!
          "main",
          statements = List(Statement.Begin(AST.newId), Statement.Return(AST.newId))
        ),
        List.empty
      )

  private val flowrunChannel = Channel[FlowRun.Event]
  private val programModel = ProgramModel(program, flowrunChannel)
  private var interpreter = Interpreter(programModel, flowrunChannel)

  private val flowchartPresenter = FlowchartPresenter(programModel, flowRunElements)
  private val functionSelector = FunctionSelector(programModel, flowrunChannel, flowRunElements)
  private val statementEditor = StatementEditor(programModel, flowrunChannel, flowRunElements)
  private val ctxMenu = CtxMenu(programModel)
  private var outputArea = OutputArea(interpreter, flowRunElements, flowrunChannel)
  private var debugArea = DebugArea(interpreter, flowRunElements)

  private var startedTime: String = ""

  flowRunElements.metaData.innerText = program.name

  functionSelector.loadFunctions()

  def json(): String =
    programModel.ast.toJson

  def funDOT(): String =
    flowchartPresenter.funDOT

  def codeText(): String =
    val generator = CodeGeneratorFactory(localConfig.get.lang, programModel.ast)
    val codeTry = generator.generate
    if codeTry.isFailure then println("Failed to generate code: " + codeTry.failed)
    codeTry.getOrElse("Error while generating code. Please fix errors in the program.")

  // run the program
  flowRunElements.runButton.onclick = _ => {
    outputArea.clearAll()
    outputArea.running()
    flowchartPresenter.clearErrors()
    flowchartPresenter.clearSelected()
    flowrunChannel := FlowRun.Event.Deselected

    startedTime = getNowTime
    flowRunElements.runtimeOutput.appendChild(div(samp(s"Started at: $startedTime"), br, br).render)

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

  import FlowRun.Event.*
  flowrunChannel.attach {
    case EvalSuccess =>
      flowRunElements.runtimeOutput.appendChild(div(samp(s"Finished at: $getNowTime")).render)
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
    case EvalError(nodeId, msg) =>
      outputArea.runtimeError(msg, startedTime, getNowTime)
      flowchartPresenter.highlightError(nodeId)
      flowchartPresenter.enable()
      functionSelector.enable()
      outputArea.finished()
    case EvalOutput(output) =>
      val newOutput = div(samp(output), br).render
      flowRunElements.runtimeOutput.appendChild(newOutput)
    case EvalInput(nodeId, name) =>
      outputArea.evalInput(nodeId, name, startedTime)
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
  }
  flowrunChannel.attach { _ =>
    // on any event hide menus
    ctxMenu.hideAllMenus()
    // gen code always
    generateCode()
  }

  // trigger first time to get the ball rolling
  flowrunChannel := SyntaxSuccess

  ///////////////////////////
  dom.window.addEventListener(
    "storage",
    (event: dom.StorageEvent) => {
      // When local storage changes set the config
      val savedConfig = dom.window.localStorage.getItem(FlowRunConfigKey)
      val savedTodos =
        if (savedConfig == null) FlowRunConfig(Language.java, "")
        else savedConfig.fromJson[FlowRunConfig]

      localConfig.set(savedTodos)
      flowrunChannel := ConfigChanged
    }
  )

  private def generateCode(): Unit = {
    // gen code always
    changeCallback match
      case None     => flowRunElements.codeArea.innerText = codeText()
      case Some(cb) => cb(this)
  }

  // TODO extract to config class
  private def initLocalConfig(): Var[FlowRunConfig] = {

    val config$ : Var[FlowRunConfig] = Var(null)
    config$.attach { newValue =>
      dom.window.localStorage.setItem(FlowRunConfigKey, newValue.toJson)
    }

    val savedConfig = dom.window.localStorage.getItem(FlowRunConfigKey)
    val savedTodos =
      if (savedConfig == null) FlowRunConfig(Language.java, "")
      else savedConfig.fromJson[FlowRunConfig]

    config$.set(savedTodos)
    config$
  }
}

object FlowRun:

  enum Event:
    case EvalSuccess
    case EvalError(nodeId: String, msg: String)
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
end FlowRun
