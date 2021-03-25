package ba.sake.flowrun

import scala.scalajs.js

extension (any: Any) {
  def asDyn: js.Dynamic = any.asInstanceOf[js.Dynamic]
}
