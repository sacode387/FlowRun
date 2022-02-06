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

  def runtimeError(msg: String, startTime: Option[String] = None, endTime: Option[String] = None): Unit =
    clearAll()
    flowRunElements.runtimeOutput.appendChild(
      div(
        startTime.map(t => samp(s"Started at: $t")),
        br,
        br,
        samp("Error: " + msg),
        br,
        br,
        endTime.map(t => samp(s"Finished at: $t"))
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
      br,
      label(
        samp(s"Please enter '$name': "),
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

        flowRunElements.runtimeOutput.removeChild(enterValueDiv)
        flowRunElements.runtimeOutput.appendChild(
          div(
            br,
            samp(s"Your entered value $name = $inputValue")
          ).render
        )
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
