package ba.sake.flowrun.eval

final class EvalException(
  message: String,
  val nodeId: String
) extends RuntimeException(message)
