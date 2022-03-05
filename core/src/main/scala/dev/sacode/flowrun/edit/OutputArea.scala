package dev.sacode.flowrun
package edit

import org.scalajs.dom
import reactify.*
import scalatags.JsDom.all.*
import dev.sacode.flowrun.eval.*
import dev.sacode.flowrun.ast.Expression.Type

class OutputArea(
    interpreter: Interpreter,
    flowRunElements: FlowRunElements,
    flowrunChannel: Channel[FlowRun.Event]
) {

  def running(): Unit =
    flowRunElements.output.classList.add("flowrun--running")

  def finished(): Unit =
    flowRunElements.output.classList.remove("flowrun--running")
    flowRunElements.output.querySelectorAll(".flowrun-user-inputs").foreach(_.remove())

  def clearAll(): Unit =
    clearStmt()
    clearSyntax()
    clearRuntime()

  def clearStmt(): Unit =
    flowRunElements.stmtOutput.innerText = ""
    flowRunElements.stmtOutput.classList.remove("flowrun--error")

  def clearSyntax(): Unit =
    flowRunElements.syntaxOutput.innerText = ""
    flowRunElements.syntaxOutput.classList.remove("flowrun--error")

  def clearRuntime(): Unit =
    flowRunElements.runtimeOutput.innerText = ""
    flowRunElements.runtimeOutput.classList.remove("flowrun--error")

  def runtimeOutput(txt: String, newline: Boolean): Unit = {
    val newOutput =
      if newline then
        if txt.isEmpty then br
        else div(samp(txt), br)
      else samp(txt)
    flowRunElements.runtimeOutput.appendChild(newOutput.render)
  }

  def runtimeError(msg: String, startTime: String, endTime: String): Unit =
    clearStmt()
    clearSyntax()
    flowRunElements.runtimeOutput.appendChild(
      div(
        samp("Error: " + msg),
        br,
        br,
        samp(s"Finished (failed) at: $endTime")
      ).render
    )
    flowRunElements.runtimeOutput.classList.add("flowrun--error")

  def syntaxError(msg: String, startTime: Option[String] = None, endTime: Option[String] = None): Unit =
    flowRunElements.syntaxOutput.innerText = ""
    flowRunElements.syntaxOutput.appendChild(samp("Syntax Error: " + msg).render)
    flowRunElements.syntaxOutput.classList.add("flowrun--error")

  def evalInput(nodeId: String, name: String): Unit = {

    val valueInputElem = flowRunElements.newInputText()
    val valueBtnElem = flowRunElements.newEnterButton
    val enterValueDiv = div(
      label(cls := "flowrun-user-inputs")(
        samp(s"Please enter '$name': "),
        valueInputElem,
        valueBtnElem
      )
    ).render
    flowRunElements.runtimeOutput.appendChild(enterValueDiv)

    def inputValueSubmitted(): Unit = {
      val inputValue = valueInputElem.value.trim
      val res = interpreter.setValue(nodeId, name, inputValue)
      res.foreach { value =>
        val printVal = if value.tpe == Type.String then s""" "$inputValue" """ else inputValue
        flowRunElements.runtimeOutput.removeChild(enterValueDiv)
        flowRunElements.runtimeOutput.appendChild(
          div(samp(s"You entered $name = $printVal")).render
        )
      }
    }

    valueInputElem.focus()
    valueInputElem.onkeydown = (event) => {
      if (event.key == "Enter") inputValueSubmitted()
    }

    valueBtnElem.onclick = _ => inputValueSubmitted()
  }
}
