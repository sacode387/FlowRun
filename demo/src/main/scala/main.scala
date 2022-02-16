import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.*
import org.scalajs.dom.document
import scalatags.JsDom.all.*
import dev.sacode.flowrun.FlowRun

@main def start(): Unit =
  dom.window.onload = _ => {

    // edit
    val flowRunEditors = document.querySelectorAll(".flowrun-instance")
    for mountElem <- flowRunEditors do
      val editable = mountElem.classList.contains("flowrun--editable")
      val flowRun: FlowRun = FlowRun(
        mountElem,
        editable,
        mountCallback = Some { fr =>
          mountElem.classList.remove("flowrun--hidden")
        },
        changeCallback = Some { fr =>

          var lang = fr.config().lang.toString
          if lang.startsWith("scala") then lang = "scala"

          val codeElem = code(cls := s"language-$lang")(
            fr.codeText()
          ).render

          val codeArea = fr.flowRunElements.codeArea
          codeArea.innerText = ""
          codeArea.appendChild(codeElem)
          js.Dynamic.global.hljs.highlightElement(codeElem)
        }
      )
    end for
  }
