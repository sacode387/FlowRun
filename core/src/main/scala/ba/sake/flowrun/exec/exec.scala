package ba.sake.flowrun
package exec

import scala.scalajs.js, js.annotation._
import org.scalajs.dom
import org.getshaka.nativeconverter.NativeConverter
import ba.sake.flowrun.eval.Interpreter

@JSExportTopLevel("Exec", Module.Exec)
object Exec {
  def main(): Unit = {
    val programModel = ProgramModel(Program())
    val interpreter = Interpreter(programModel)
    WebWorkerGlobals.onmessage = (e: dom.MessageEvent) => {
      import Request._
      
      val req = NativeConverter[Request].fromNative(e.data.asDyn)
      req match
        case Run() =>
          interpreter.run()
        case req: Delete =>
          programModel.delete(req)
        case req: AddDeclare =>
          programModel.addDeclare(req)
        case req: AddAssign =>
          programModel.addAssign(req)
        case req: AddOutput =>
          programModel.addOutput(req)
        case req: AddInput =>
          programModel.addInput(req)
        case req: AddIf =>
          programModel.addIf(req)
        case req: UpdateDeclare =>
          programModel.updateDeclare(req)
        case req: UpdateAssign =>
          programModel.updateAssign(req)
        case req: UpdateOutput =>
          programModel.updateOutput(req)
        case req: UpdateIf =>
          programModel.updateIf(req)
    }
  }
}

@js.native
@JSGlobalScope
object WebWorkerGlobals extends js.Any {
  def postMessage(aMessage: js.Any): Unit = js.native
  var onmessage: js.Function1[dom.MessageEvent, _] = js.native
}