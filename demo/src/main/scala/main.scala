import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.*
import org.scalajs.dom.document
import scalatags.JsDom.all.*
import dev.sacode.flowrun.FlowRun

@main def start(): Unit =
  dom.window.onload = _ => {

    val flowRunMounts = document.querySelectorAll(".flowrun-editor")
    for mountElem <- flowRunMounts do
      // init FlowRun
      val flowRun: FlowRun = FlowRun(
        mountElem,
        changeCallback = Some(fr => {
          val codeArea = fr.flowRunElements.codeArea
          codeArea.innerText = ""
          var lang = fr.config().lang.toString
          if lang.startsWith("scala") then lang = "scala"
          val codeElem = code(cls := s"language-$lang")(
            fr.codeText()
          ).render
          codeArea.appendChild(codeElem)
          js.Dynamic.global.hljs.highlightElement(codeElem)
        })
      )
    end for
  }
