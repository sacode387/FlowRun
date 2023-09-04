package dev.sacode.flowrun

import scalajs.js
import js.annotation.*
import org.scalajs.dom

@js.native
@JSGlobalScope
object toastify extends js.Object {

  def Toastify(options: ToastifyOptions): Toast = js.native
}

@js.native
trait ToastifyOptions extends js.Object {
  val text: String = js.native
  val gravity: String = js.native // top bottom
  val position: String = js.native // left right center
}

object ToastifyOptions {
  def apply(text: String): ToastifyOptions = apply(text, "bottom", "center", Color.blue)

  def apply(text: String, color: Color): ToastifyOptions = apply(text, "bottom", "center", color)

  def apply(text: String, gravity: String, position: String, color: Color): ToastifyOptions = {
    js.Dynamic
      .literal(
        text = text,
        gravity = gravity,
        position = position,
        style = js.Dynamic.literal(
          background = color.fill,
          color = color.font
        ),
        offset = js.Dynamic.literal(
          y = "5rem"
        )
      )
      .asInstanceOf[ToastifyOptions]
  }
}

@js.native
trait Toast extends js.Object {
  def showToast(): Unit = js.native
}
