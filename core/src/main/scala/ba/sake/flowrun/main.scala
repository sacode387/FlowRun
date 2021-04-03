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
    val runBtn = document.getElementById("run-btn").asInstanceOf[dom.html.Button]
    val editWrapperElem = document.getElementById("edit-wrapper")

    val programModel = ProgramModel(Program())
    val interpreter = Interpreter(programModel)
    val cytoscapeFlowchart = CytoscapeFlowchart(programModel, container, editWrapperElem)

    runBtn.onclick = _ => {
      cytoscapeFlowchart.clearErrors()
      outputElem.innerText = ""
      interpreter.run()
    }

    document.getElementById("continue-btn")
      .asInstanceOf[dom.html.Button].onclick = _ => {
        interpreter.continue()
      }

    dom.document.addEventListener("eval-error", (e: dom.CustomEvent) => {
      outputElem.innerText = "Runtime error: " + e.detail.asDyn.msg
      outputElem.classList.add("error")
    })
    dom.document.addEventListener("eval-output", (e: dom.CustomEvent) => {
      val newOutput = dom.document.createElement("pre")
      newOutput.innerText = e.detail.asDyn.output.toString
      outputElem.appendChild(newOutput)
    })
  }
}
