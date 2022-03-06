package dev.sacode.flowrun
package edit

import org.scalajs.dom
import reactify.*
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
        td(sym.value.flatMap(_.valueOpt.map(_.toString)).getOrElse(""))
      )
    }
    flowRunElements.debugVariables.appendChild(
      table(
        tr(th("Name"), th("Value")),
        rows
      ).render
    )

}
