package dev.sacode.flowrun
package edit

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scalajs.js
import scalajs.js.JSConverters.*
import org.scalajs.dom
import reactify.*
import dev.sacode.flowrun.parse.*
import dev.sacode.flowrun.ast.*, Expression.Type, Statement.*
import dev.sacode.flowrun.eval.Interpreter.ExecMode
import dev.sacode.flowrun.formatgen.DotGenerator

/*
- color==border_color, fillcolor==.. https://stackoverflow.com/questions/9106079/graphviz-how-to-change-border-color
 */
class FlowchartPresenter(
    programModel: ProgramModel,
    flowRunElements: FlowRunElements,
    flowRunTheme: FlowRunTheme,
    flowrunChannel: Channel[FlowRun.Event]
) {

  private val graphviz = d3
    .select(flowRunElements.drawArea)
    .graphviz(
      js.Dynamic.literal(
        zoom = true,
        fit = true
      )
    )

  graphviz.on("end", { (thiz: js.Dynamic) =>
    thiz.zoomSelection().on("dblclick.zoom", null)
  }: js.ThisFunction0[js.Dynamic, Unit])

  loadCurrentFunction()

  def disable(mode: ExecMode): Unit =
    flowRunElements.drawArea.classList.add("flowrun--disabled")
    if mode == ExecMode.NORMAL then flowRunElements.runButton.classList.add("flowrun--disabled")
    flowRunElements.stopButton.classList.remove("flowrun--disabled")

  def enable(): Unit =
    flowRunElements.drawArea.classList.remove("flowrun--disabled")
    flowRunElements.runButton.classList.remove("flowrun--disabled")
    flowRunElements.stopButton.classList.add("flowrun--disabled")

  def clearErrors(): Unit =
    dom.window.document.querySelectorAll(".node ").foreach(_.classList.remove("flowrun--error"))

  def clearSelected(): Unit =
    dom.window.document.querySelectorAll(".flowrun--selected").foreach(_.classList.remove("flowrun--selected"))

  def highlightError(nodeId: String): Unit =
    if nodeId != null && nodeId.trim.nonEmpty then
      dom.window.document.querySelector(s""" .node[id^="$nodeId"] """).classList.add("flowrun--error")

  def highlightExecuting(nodeIdOpt: Option[String]): Unit =
    val cls = "flowrun--to-execute"
    dom.window.document.querySelectorAll(".node").foreach(_.classList.remove(cls))
    nodeIdOpt.foreach { nodeId =>
      val domNode = dom.window.document.querySelector(s""" .node[id^="${nodeId}"] """)
      if domNode != null && !js.isUndefined(domNode) then domNode.classList.add(cls)
    }

  def loadCurrentFunction(): Future[Unit] = {
    val p = Promise[Unit]()
    graphviz
      .engine("neato")
      .renderDot(
        funDOT,
        (gr: js.Dynamic) => {
          clearSelected()
          clearErrors()
          programModel.currentSelectedStmtId.foreach { id =>
            dom.window.document
              .querySelectorAll(s""" .node[id*="${id}"] """)
              .foreach(_.classList.add("flowrun--selected"))
          }
          flowrunChannel := FlowRun.Event.SvgMounted
          p.success(())
        }
      )
    p.future
  }

  def funDOT: String =
    val dotGenerator = DotGenerator(programModel.currentFunction, flowRunTheme)
    dotGenerator.generate

}
