
import scala.scalajs.js
import org.scalajs.dom.ext._
import org.scalajs.dom
import org.scalajs.dom.document
import ba.sake.flowrun.FlowRun

@main def start(): Unit =
  dom.window.onload = _ => { 

    val flowRunMounts = document.querySelectorAll(".FlowRun-editor")
    for flowRunMount <- flowRunMounts do
      val flowRun = FlowRun(flowRunMount.asInstanceOf[dom.Element])

   // println(js.JSON.stringify(flowRun.json()))
  }

