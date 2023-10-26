package dev.sacode.flowrun

import scala.scalajs.js

object Fixtures {
  val setInterval: (Long, => Unit) => Any =
    (interval, body) => js.timers.setInterval(interval.toDouble)(body)

  val clearInterval: Any => Unit =
    (handle) => js.timers.clearInterval(handle.asInstanceOf[js.timers.SetIntervalHandle])
}
