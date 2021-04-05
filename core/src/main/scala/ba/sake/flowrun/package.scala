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

object TypeUtils:
  import Expression.Type
  // check if assignable, and optionally casts the type
  def getUpdateValue(nodeId: String, name: String, expectedType: Type, value: Any): Any = {
    println(s"UPDATE: $expectedType, $value, ${value.getClass}")
    (expectedType, value) match {
      case (Type.Integer, _: Int) => value
      case (Type.Real, _: Double) => value
      case (Type.String, _: String) => value
      case (Type.Boolean, _: Boolean) => value
      case (Type.Void, _) => ()
      case (expectedType, _) =>
        throw eval.EvalException(
          s"Cannot assign value of type '${value.getClass.getSimpleName}' to a variable of type '$expectedType'", nodeId)
    }
  }