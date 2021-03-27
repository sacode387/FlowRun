package ba.sake.flowrun

import org.scalajs.dom
import org.scalajs.dom.window
import org.scalajs.dom.window.document
import ba.sake.flowrun.cytoscape.CytoscapeFlowchart
import ba.sake.flowrun.eval.Interpreter

@main def start(): Unit = {
  
  window.onload = (e: dom.Event) => {
    
    val container = document.getElementById("cy")
    val outputElem = document.getElementById("run-output")
    val runBtn = document.getElementById("run-btn").asInstanceOf[dom.html.Button]
    val editWrapperElem = dom.window.document.getElementById("edit-wrapper")

    val programModel = ProgramModel(Program())
    val cytoscapeFlowchart = CytoscapeFlowchart(container, editWrapperElem, programModel)

    runBtn.onclick = _ => {
      cytoscapeFlowchart.clearErrors()
      Interpreter(programModel, outputElem).run()
    }
  }
}
