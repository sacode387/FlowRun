package dev.sacode.flowrun
package edit

import org.scalajs.dom
import reactify.*
import scalatags.JsDom.all.*

class FunctionSelector(
    programModel: ProgramModel,
    flowrunChannel: Channel[FlowRun.Event],
    flowRunElements: FlowRunElements
) {

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
        label(radio, span(s" ${f.name}")),
        Option.unless(f.isMain)(deleteFunButton)
      )
    }

    flowRunElements.functionsChooser.innerText = ""
    flowRunElements.functionsChooser.appendChild(
      functionInputs.render
    )
  end loadFunctions

}
