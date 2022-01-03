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
    val functionSelector = flowRunElements.newInputSelect
    functionSelector.name = s"${programModel.ast.id}-currentFunction"
    functionSelector.onchange = { (e: dom.Event) =>
      val selectedFunId = e.target.asInstanceOf[dom.html.Input].value
      programModel.currentFunctionId = selectedFunId
      flowrunChannel := FlowRun.Event.FunctionSelected
    }
    allFunctions.foreach { f =>
      val maybeSelected = Option.when(f.id == programModel.currentFunctionId)(selected)
      val funItem = option(value := f.id, maybeSelected)(f.name).render
      functionSelector.add(funItem)
    }

    val deleteFunButton = flowRunElements.deleteFunButton
    deleteFunButton.onclick = { (e: dom.Event) =>
      programModel.deleteFunction(programModel.currentFunctionId)
    }

    val selectElem = frag(
      label("Function: "),
      functionSelector,
      flowRunElements.addFunButton,
      Option.unless(programModel.currentFunction.isMain)(deleteFunButton)
    )
    flowRunElements.functionsChooser.innerText = ""
    flowRunElements.functionsChooser.appendChild(selectElem.render)
  end loadFunctions
}
