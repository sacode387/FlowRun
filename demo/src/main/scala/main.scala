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
        changeCallback = Some((fr, codeText) => {
          val codeArea = fr.flowRunElements.codeArea
          codeArea.innerText = ""
          val codeElem = code(cls := "language-scala")(codeText).render
          codeArea.appendChild(codeElem)
          js.Dynamic.global.hljs.highlightElement(codeElem)
        })
      )

      // setup tab button
      val flipTabButton: dom.html.Element = mountElem.querySelector(".flip-tab").asInstanceOf[dom.html.Button]
      flipTabButton.onclick = _ => {
        val aktivan = "flowrun-active"
        mountElem.querySelectorAll(".flowrun-tabs > *").foreach { tab =>
          if tab.classList.contains(aktivan) then tab.classList.remove(aktivan)
          else tab.classList.add(aktivan)
        }
      }
    end for
  }
