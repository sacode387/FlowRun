package dev.sacode.flowrun

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.ScheduledFuture

object Fixtures {
  private val pool = Executors.newScheduledThreadPool(3)

  val setInterval: (Long, => Unit) => Any =
    (interval, body) => pool.scheduleAtFixedRate(() => body, 0, interval, TimeUnit.MILLISECONDS)

  val clearInterval: Any => Unit =
    (handle) => handle.asInstanceOf[ScheduledFuture[?]].cancel(true)

}
