package dev.sacode.flowrun
package edit

import org.scalajs.dom
import reactify.*
import scalatags.JsDom.all.*

class FunctionSelector(
    editable: Boolean,
    programModel: ProgramModel,
    flowrunChannel: Channel[FlowRun.Event],
    flowRunElements: FlowRunElements
) {

  def disable(): Unit =
    flowRunElements.functionsList.classList.add("flowrun--disabled")

  def enable(): Unit =
    flowRunElements.functionsList.classList.remove("flowrun--disabled")

  def allFunctions = List(programModel.ast.main) ++ programModel.ast.functions

  def loadFunctions(): Unit =
    val functionInputs = allFunctions.map { f =>
      val radio = flowRunElements.newInputRadio
      radio.id = f.id
      radio.value = f.id
      radio.name = s"${programModel.ast.id}-currentFunction"
      radio.checked = f.id == programModel.currentFunctionId
      radio.onchange = { (e: dom.Event) =>
        programModel.currentFunctionId = f.id
        flowrunChannel := FlowRun.Event.FunctionSelected
      }

      val deleteFunButton = flowRunElements.newDeleteFunButton
      deleteFunButton.onclick = { (e: dom.Event) =>
        programModel.deleteFunction(f.id)
      }

      div(
        label(title := f.verboseLabel)(radio, span(f.label)),
        Option.when(!f.isMain && editable)(deleteFunButton)
      )
    }

    flowRunElements.functionsChooser.innerText = ""
    flowRunElements.functionsChooser.appendChild(
      functionInputs.render
    )
  end loadFunctions

}
