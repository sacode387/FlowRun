package ba.sake.flowrun

import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.scalajs.dom.window
import org.scalajs.dom.window.document
import org.getshaka.nativeconverter.NativeConverter
import ba.sake.flowrun.cytoscape.CytoscapeFlowchart
import exec._

@JSExportTopLevel("start")
@main def start(): Unit = {

  val execWorker = new WebWorker(
    "scripts/exec.js", 
    js.Dynamic.literal(`type` = "module")
  )

  window.onload = (_: dom.Event) => {
    
    val container = document.getElementById("cy")
    val outputElem = document.getElementById("run-output")
    val runBtn = document.getElementById("run-btn").asInstanceOf[dom.html.Button]
    val editWrapperElem = document.getElementById("edit-wrapper")

    val cytoscapeFlowchart = CytoscapeFlowchart(container, editWrapperElem, execWorker)

    runBtn.onclick = _ => {
      cytoscapeFlowchart.clearErrors()
      outputElem.innerText = ""
      sendToWorker(Request.Run())
    }

    execWorker.onmessage = { (e: dom.MessageEvent) =>
      import Response._

      val res = NativeConverter[Response].fromNative(e.data.asDyn)
      res match
        case res: Output =>
          val newOutput = dom.document.createElement("pre")
          newOutput.innerText = res.str
          outputElem.appendChild(newOutput)
        case res: Finished =>
          // noop
        case res: Failed =>
          outputElem.innerText = "Runtime error: " + res.msg
          outputElem.classList.add("error")

          val errorEvent = new dom.CustomEvent("eval-error",
            js.Dynamic.literal(detail = 
              js.Dynamic.literal(nodeId = res.nodeId)
            ).asInstanceOf[dom.raw.CustomEventInit]
          )
          dom.document.dispatchEvent(errorEvent)
    }
  }

  def sendToWorker(req: Request): Unit =
    execWorker.postMessage(req.toNative)
}

// ScalaJS-DOM is missing the params
@js.native
@JSGlobal("Worker")
class WebWorker(stringUrl: String, params: js.Any) extends js.Object {
  def postMessage(aMessage: js.Any): Unit = js.native
  var onmessage: js.Function1[dom.MessageEvent, _] = js.native
}