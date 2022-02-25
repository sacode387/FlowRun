import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.*
import org.scalajs.dom.document
import scalatags.JsDom.all.*
import dev.sacode.flowrun.{FlowRun, ColorScheme}

@main def start(): Unit =
  dom.window.onload = _ => {

    val colorScheme = ColorScheme.default.withFontName("Courier Prime")
    val flowRunEditors = document.querySelectorAll(".flowrun-instance")
    for elem <- flowRunEditors do
      val mountElem = elem.asInstanceOf[dom.html.Element]
      val editable = mountElem.classList.contains("flowrun--editable")
      val flowRun: FlowRun = FlowRun(
        colorScheme = colorScheme,
        mountElem = mountElem,
        editable = editable,
        mountCallback = { (fr: FlowRun) =>
          mountElem.classList.remove("flowrun--hidden")
        },
        changeCallback = { (fr: FlowRun) =>

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
