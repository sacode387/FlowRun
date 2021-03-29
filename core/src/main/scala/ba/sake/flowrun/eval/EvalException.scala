package ba.sake.flowrun
package eval

import scalajs.js.annotation._

@JSExportTopLevel("EvalException", Module.Exec)
final class EvalException(
  message: String,
  val nodeId: String
) extends RuntimeException(message)
