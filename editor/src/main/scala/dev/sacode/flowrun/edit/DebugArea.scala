package dev.sacode.flowrun
package edit

import scalatags.JsDom.all.*
import dev.sacode.flowrun.eval.*

class DebugArea(
    interpreter: Interpreter,
    flowRunElements: FlowRunElements
) {

  def clear(): Unit =
    flowRunElements.debugVariables.innerText = ""

  def showVariables(): Unit =
    flowRunElements.debugVariables.innerText = ""
    val varValues = interpreter.symTab.varSymbols
    val rows = varValues.map { sym =>
      tr(
        td(sym.key.name),
        td(sym.value.map(_.valueString).getOrElse(""))
      )
    }
    flowRunElements.debugVariables.appendChild(
      table(
        tr(th("Name"), th("Value")),
        rows
      ).render
    )

}
