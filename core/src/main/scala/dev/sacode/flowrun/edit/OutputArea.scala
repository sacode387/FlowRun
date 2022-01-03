package dev.sacode.flowrun
package edit

import org.scalajs.dom
import reactify.*
import scalatags.JsDom.all.*
import dev.sacode.flowrun.eval.*

class OutputArea(
    interpreter: Interpreter,
    flowRunElements: FlowRunElements
) {

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
    flowRunElements.runtimeOutput.classList.remove("flowrun--success")

  def runtimeError(msg: String, startTime: Option[String] = None, endTime: Option[String] = None): Unit =
    clearAll()
    flowRunElements.runtimeOutput.appendChild(
      div(
        startTime.map(t => div(s"Started at: $t")),
        br,
        pre("Error: " + msg),
        br,
        endTime.map(t => div(s"Finished at: $t"))
      ).render
    )
    flowRunElements.runtimeOutput.classList.add("flowrun--error")

  def syntaxError(msg: String, startTime: Option[String] = None, endTime: Option[String] = None): Unit =
    flowRunElements.syntaxOutput.innerText = "Syntax Error: " + msg
    flowRunElements.syntaxOutput.classList.add("flowrun--error")

  def evalInput(nodeId: String, name: String): Unit = {

    val valueInputElem = flowRunElements.newInputText
    val valueBtnElem = flowRunElements.newEnterButton
    val enterValueDiv = div(
      label(
        pre(s"Please enter value for variable '$name': "),
        valueInputElem,
        valueBtnElem
      )
    ).render
    flowRunElements.runtimeOutput.appendChild(enterValueDiv)

    def inputValueSubmitted(): Unit = {
      val inputValue = valueInputElem.value.trim
      val key = SymbolKey(name, Symbol.Kind.Variable, nodeId)
      val sym = interpreter.symTab.getSymbol(null, key)
      try {
        val value = sym.tpe match
          case Expression.Type.Integer => inputValue.toInt
          case Expression.Type.Real    => inputValue.toDouble
          case Expression.Type.Boolean => inputValue.toBoolean
          case Expression.Type.String  => inputValue
          case Expression.Type.Void    => ()
        interpreter.symTab.setValue(nodeId, name, value)
        interpreter.continue()

        val newOutput = pre(s"Your entered value $name = $inputValue").render
        flowRunElements.runtimeOutput.removeChild(enterValueDiv)
        flowRunElements.runtimeOutput.appendChild(newOutput)
      } catch {
        case (e: EvalException) => // from symbol table
          runtimeError(e.getMessage)
        case e: (NumberFormatException | IllegalArgumentException) =>
          runtimeError(s"Entered invalid ${sym.tpe}: '${inputValue}'")
      }
    }

    valueInputElem.focus()
    valueInputElem.onkeydown = (event) => {
      if (event.key == "Enter") inputValueSubmitted()
    }

    valueBtnElem.onclick = _ => inputValueSubmitted()
  }
}
