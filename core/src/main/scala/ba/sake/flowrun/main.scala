package ba.sake.flowrun

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.window
import org.scalajs.dom.window.document
import org.getshaka.nativeconverter.NativeConverter
import ba.sake.flowrun.cytoscape.CytoscapeFlowchart
import ba.sake.flowrun.eval._

@JSExportTopLevel("start")
@main def start(): Unit = {

  window.onload = (_: dom.Event) => {
    
    val container = document.getElementById("cy")
    val outputElem = document.getElementById("run-output")
    val variablesElem = document.getElementById("variables-output")
    val runBtn = document.getElementById("run-btn").asInstanceOf[dom.html.Button]
    val editWrapperElem = document.getElementById("edit-wrapper")

    val programModel = ProgramModel(Program())
    val cytoscapeFlowchart = CytoscapeFlowchart(programModel, container, editWrapperElem)

    var interpreter = Interpreter(programModel)

    runBtn.onclick = _ => {
      cytoscapeFlowchart.clearErrors()
      outputElem.innerText = s"Started at: $getNowTime"

      interpreter = Interpreter(programModel) // fresh SymTable etc
      interpreter.run()
    }

    dom.document.addEventListener("eval-error", (e: dom.CustomEvent) => {
      outputElem.innerText += "\nRuntime error: " + e.detail.asDyn.msg
      outputElem.classList.add("error")
    })

    dom.document.addEventListener("eval-output", (e: dom.CustomEvent) => {
      val newOutput = dom.document.createElement("pre")
      newOutput.innerText = e.detail.asDyn.output.toString
      outputElem.appendChild(newOutput)
    })

    dom.document.addEventListener("eval-input", (e: dom.CustomEvent) => {
      val nodeId = e.detail.asDyn.nodeId.toString
      val name = e.detail.asDyn.name.toString

      val valueLabelElem = dom.document.createElement("label").asInstanceOf[dom.html.Label]
      val valueInputElem = dom.document.createElement("input").asInstanceOf[dom.html.Input]

      val valueBtnElem = dom.document.createElement("button").asInstanceOf[dom.html.Button]
      valueBtnElem.innerHTML = "Enter"

      valueLabelElem.innerText = s"Please enter value for '$name': "
      valueLabelElem.appendChild(valueInputElem)
      valueLabelElem.appendChild(valueBtnElem)
      outputElem.appendChild(valueLabelElem)
      valueInputElem.focus()

      valueBtnElem.onclick = _ => {
        val inputValue = valueInputElem.value.trim
        try {
          interpreter.symTab.set(nodeId, name, inputValue)
          interpreter.continue()
          valueLabelElem.removeChild(valueBtnElem)
          valueLabelElem.removeChild(valueInputElem)
          valueLabelElem.innerText = valueLabelElem.innerText + " " + inputValue
        } catch {
          case (e: EvalException) =>
            EventUtils.dispatchEvent("eval-error",
              js.Dynamic.literal(msg = e.getMessage, nodeId = e.nodeId)
            )
        }
      }
    })

    dom.document.addEventListener("eval-var-updated", (e: dom.CustomEvent) => {
      showVariables()
    })

    def showVariables(): Unit =
      variablesElem.innerText = ""
      interpreter.symTab.symbols.values.foreach { sym =>
        val symElem = dom.document.createElement("div")
        symElem.innerText = s"${sym.name}: ${sym.tpe} = ${sym.value.getOrElse("")}"
        variablesElem.appendChild(symElem)
      }
    
    def getNowTime: String =
      val now = new js.Date()
      now.toLocaleTimeString
  }
}
