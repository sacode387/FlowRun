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
    flowRunElements.output.innerText = ""
    flowRunElements.output.classList.remove("error")

  def displayError(msg: String): Unit =
    flowRunElements.output.innerText = msg
    flowRunElements.output.classList.add("error")
  
  def evalInput(nodeId: String, name: String) = {

    val valueInputElem = flowRunElements.newInputText
    val valueBtnElem = flowRunElements.newEnterButton
    val enterValueDiv = div(
      label(
        s"Please enter value for '$name': ",
        valueInputElem,
        valueBtnElem
      )
    ).render
    flowRunElements.output.appendChild(enterValueDiv)

    valueInputElem.focus()

    valueBtnElem.onclick = _ => {
      val inputValue = valueInputElem.value.trim
      val key = SymbolKey(name, Symbol.Kind.Variable)
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

        val newOutput = pre(s"Please enter value for '$name': $inputValue").render
        flowRunElements.output.removeChild(enterValueDiv)
        flowRunElements.output.appendChild(newOutput)
      } catch {
        case (e: EvalException) => // from symbol table
          displayError(e.getMessage)
        case e: (NumberFormatException | IllegalArgumentException) =>
          displayError(s"Entered invalid ${sym.tpe}: '${inputValue}'")
      }
    }
  }
}
