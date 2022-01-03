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

  def clearErrors(): Unit =
    flowRunElements.scratchpad.innerText = ""
    flowRunElements.scratchpad.classList.remove("error")

  def displayError(msg: String): Unit =
    flowRunElements.scratchpad.innerText = msg
    flowRunElements.scratchpad.classList.add("error")

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
    flowRunElements.scratchpad.appendChild(enterValueDiv)

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
        flowRunElements.scratchpad.removeChild(enterValueDiv)
        flowRunElements.scratchpad.appendChild(newOutput)
      } catch {
        case (e: EvalException) => // from symbol table
          displayError(e.getMessage)
        case e: (NumberFormatException | IllegalArgumentException) =>
          displayError(s"Entered invalid ${sym.tpe}: '${inputValue}'")
      }
    }

    valueInputElem.focus()
    valueInputElem.onkeydown = (event) => {
      if (event.key == "Enter") inputValueSubmitted()
    }

    valueBtnElem.onclick = _ => inputValueSubmitted()
  }
}
