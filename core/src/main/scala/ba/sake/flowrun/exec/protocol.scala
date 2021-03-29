package ba.sake.flowrun
package exec

import scalajs.js
import scalajs.js.annotation._

import org.getshaka.nativeconverter.NativeConverter

import ba.sake.flowrun.Expression
import ba.sake.flowrun.Expression.Type

enum Request derives NativeConverter:
  case Run()
  case Delete(id: String)
  case AddDeclare(id: String, name: String, tpe: Type, afterId: String, blockId: String)
  case AddAssign(id: String, afterId: String, blockId: String)
  case AddOutput(id: String, afterId: String, blockId: String)
  case AddInput(id: String, afterId: String, blockId: String)
  case AddIf(id: String, trueId: String, falseId: String, endId: String, afterId: String, blockId: String)
  case UpdateDeclare(id: String, name: Option[String] = None, tpe: Option[Type] = None, expr: Option[Expression] = None)
  case UpdateAssign(id: String, name: Option[String] = None, expr: Option[Expression] = None)
  case UpdateOutput(id: String, expr: Expression)
  case UpdateIf(id: String, expr: Expression)

enum Response derives NativeConverter:
  case Output(str: String)
  case Finished() 
  case Failed(msg: String, nodeId: String)





/*
sealed trait Request extends js.Object:
  val reqTpe: String

/* Class gets lost in transmission main <-> web-worker,
 * thus we have to use JSON.
 */
object Request:
  
  class Run extends Request:
    val reqTpe = Run.reqTpe
  object Run:
    private val reqTpe = "Run"
    def unapply(req: Request): Option[Run] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[Run])

  class Delete(val id: String) extends Request:
    val reqTpe = Delete.reqTpe
  object Delete:
    private val reqTpe = "Delete"
    def unapply(req: Request): Option[Delete] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[Delete])

  class AddDeclare(val id: String, val name: String, val tpe: Type)(val afterId: String, val blockId: String) extends Request:
    val reqTpe = AddDeclare.reqTpe
  object AddDeclare:
    private val reqTpe = "AddDeclare"
    def unapply(req: Request): Option[AddDeclare] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[AddDeclare])

  class AddAssign(val id: String)(val afterId: String, val blockId: String) extends Request:
    val reqTpe = AddAssign.reqTpe
  object AddAssign:
    private val reqTpe = "AddAssign"
    def unapply(req: Request): Option[AddAssign] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[AddAssign])

  class AddOutput(val id: String)(val afterId: String, val blockId: String) extends Request:
    val reqTpe = AddOutput.reqTpe
  object AddOutput:
    private val reqTpe = "AddOutput"
    def unapply(req: Request): Option[AddOutput] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[AddOutput])

  class AddInput(val id: String)(val afterId: String, val blockId: String) extends Request:
    val reqTpe = AddInput.reqTpe
  object AddInput:
    private val reqTpe = "AddInput"
    def unapply(req: Request): Option[AddInput] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[AddInput])

  class AddIf(val id: String, val trueId: String, val falseId: String, val endId: String)(val afterId: String, val blockId: String) extends Request:
    val reqTpe = AddIf.reqTpe
  object AddIf:
    private val reqTpe = "AddIf"
    def unapply(req: Request): Option[AddIf] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[AddIf])


  class UpdateDeclare(val id: String, val name: Option[String] = None, val tpe: Option[Type] = None, val expr: Option[Expression] = None) extends Request:
    val reqTpe = UpdateDeclare.reqTpe
  object UpdateDeclare:
    private val reqTpe = "UpdateDeclare"
    def unapply(req: Request): Option[UpdateDeclare] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[UpdateDeclare])

  class UpdateAssign(val id: String, val name: Option[String] = None, val expr: Option[Expression] = None) extends Request:
    val reqTpe = UpdateAssign.reqTpe
  object UpdateAssign:
    private val reqTpe = "UpdateAssign"
    def unapply(req: Request): Option[UpdateAssign] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[UpdateAssign])

  class UpdateOutput(val id: String, val expr: Expression) extends Request:
    val reqTpe = UpdateOutput.reqTpe
  object UpdateOutput:
    private val reqTpe = "UpdateOutput"
    def unapply(req: Request): Option[UpdateOutput] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[UpdateOutput])

  class UpdateIf(val id: String, val expr: Expression) extends Request:
    val reqTpe = UpdateIf.reqTpe
  object UpdateIf:
    private val reqTpe = "UpdateIf"
    def unapply(req: Request): Option[UpdateIf] =
      Option.when(req.reqTpe == reqTpe)(req.asInstanceOf[UpdateIf])

end Request


sealed trait Response extends js.Object:
  val resTpe: String

object Response:

  class Output(val str: String) extends Response:
    val resTpe = Output.resTpe
  object Output:
    private val resTpe = "Output"
    def unapply(req: Response): Option[Output] =
      Option.when(req.resTpe == resTpe)(req.asInstanceOf[Output])
  
  class Finished() extends Response:
    val resTpe = Finished.resTpe
  object Finished:
    private val resTpe = "Finished"
    def unapply(req: Response): Option[Finished] =
      Option.when(req.resTpe == resTpe)(req.asInstanceOf[Finished])
  
  class Failed(val msg: String, val nodeId: String) extends Response:
    val resTpe = Failed.resTpe
  object Failed:
    private val resTpe = "Failed"
    def unapply(req: Response): Option[Failed] =
      Option.when(req.resTpe == resTpe)(req.asInstanceOf[Failed])


*/