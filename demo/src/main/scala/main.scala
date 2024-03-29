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
          // JSON text is hidden
          // only when program is mounted then we show that DIV
          mountElem.classList.remove("flowrun--hidden")
        }
      )
    end for
  }
