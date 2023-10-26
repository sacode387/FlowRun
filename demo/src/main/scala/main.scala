import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.*
import org.scalajs.dom.document
import scalatags.JsDom.all.*

import dev.sacode.flowrun.*

@main def start(): Unit =
  dom.window.onload = _ => {

    val colorScheme = ColorScheme.default.withFontName("Courier Prime")
    val flowRunEditors = document.querySelectorAll(".flowrun-instance")
    for elem <- flowRunEditors do
      val mountElem = elem.asInstanceOf[dom.html.Element]
      val flowRunEditor: FlowRunEditor = FlowRunEditor(
        colorScheme = colorScheme,
        mountElem = mountElem,
        mountCallback = { fr =>
          // JSON text is hidden
          // only when program is mounted then we show that DIV
          mountElem.classList.remove("flowrun--hidden")
        }
      )
    end for
  }
