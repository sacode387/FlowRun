package dev.sacode.flowrun.eval

final class EvalException(
  message: String,
  val nodeId: String
) extends RuntimeException(message)
