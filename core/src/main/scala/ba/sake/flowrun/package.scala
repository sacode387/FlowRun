package ba.sake.flowrun

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.window

extension (any: Any) {
  def asDyn: js.Dynamic = any.asInstanceOf[js.Dynamic]
}

object EventUtils:
  def dispatchEvent(tpe: String, payload: js.Any): Unit =
    val event = new dom.CustomEvent(
      tpe,
      js.Dynamic.literal(detail = payload).asInstanceOf[dom.raw.CustomEventInit]
    )
    dom.document.dispatchEvent(event)