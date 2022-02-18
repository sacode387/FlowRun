package dev.sacode.flowrun
package edit

import java.util.UUID

import scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.{name => _, *}
import reactify.*
import dev.sacode.flowrun.ProgramModel.Request
import dev.sacode.flowrun.parse.*
import dev.sacode.flowrun.toastify.*
import dev.sacode.flowrun.ast.*

/** Editor for selected statement. */
class StatementEditor(
    programModel: ProgramModel,
    flowrunChannel: Channel[FlowRun.Event],
    flowRunElements: FlowRunElements
) {

  def edit(stmtId: String): Unit = {

    val stmt = programModel.findStatement(stmtId)
    val stmtType = stmt.getClass.getSimpleName.filterNot(_ == '$')

    stmt match
      case _: Statement.Begin if programModel.currentFunction.isMain =>
        // skip Begin if main function
        Toastify(ToastifyOptions("Begin is not editable.", Color.yellow)).showToast()
        return
      case _: Statement.Return =>
        // skip Return if function doesn't return anything
        if programModel.currentFunction.isMain then
          Toastify(ToastifyOptions("End is not editable.", Color.yellow)).showToast()
          return
        else if programModel.currentFunction.tpe == Expression.Type.Void then
          Toastify(ToastifyOptions("Void function does not return any value.", Color.yellow)).showToast()
          return
      case _ => ()

    // clear first, prepare for new inputs
    flowRunElements.stmtOutput.innerText = ""

    // fill in this element with name/type/expression, coz it's just one line (flex)
    val stmtElems = div(cls := "flowrun-stmt-inputs").render

    // name input
    val nameInputSize = if stmtType == "ForLoop" then 3 else 10
    val nameInputElem = flowRunElements.newInputText(nameInputSize)
    nameInputElem.value = Statement.name(stmt, programModel.currentFunction.name)
    nameInputElem.placeholder = if stmtType == "Begin" then "myFun" else "x"
    nameInputElem.oninput = { (_: dom.Event) =>
      val newName = nameInputElem.value.trim
      val errorMsg: Option[String] = NameUtils.validateIdentifier(newName)
      errorMsg match
        case None =>
          stmt match
            case _: Statement.Declare =>
              programModel.updateDeclare(Request.UpdateDeclare(stmtId, name = Some(newName)))
            case _: Statement.Input =>
              programModel.updateInput(Request.UpdateInput(stmtId, name = newName))
            case _: Statement.Begin =>
              programModel.updateFunction(Request.UpdateFunction(stmtId, name = Some(newName)))
            case _: Statement.Assign =>
              programModel.updateAssign(Request.UpdateAssign(stmtId, name = Some(newName)))
            case _: Statement.ForLoop =>
              programModel.updateForLoop(Request.UpdateForLoop(stmtId, varName = Some(newName)))
            case _ => ()
        case Some(msg) =>
          flowrunChannel := FlowRun.Event.SyntaxError(msg)
    }

    var filledName = false
    val hasName = Statement.hasName(stmt, programModel.currentFunction.name)
    if hasName then
      filledName = nameInputElem.value.nonEmpty
      stmtElems.appendChild(nameInputElem)

    // type input
    val typeSelectElem = flowRunElements.newInputSelect
    val types =
      if stmtType == "Begin" then Expression.Type.values
      else Expression.Type.VarTypes
    types.foreach { tpe =>
      val typeItem = option(value := tpe.toString)(tpe.toString).render
      typeSelectElem.add(typeItem)
    }
    typeSelectElem.onchange = { (e: dom.Event) =>
      val thisElem = e.target.asInstanceOf[dom.html.Select]
      val newType = Expression.Type.valueOf(thisElem.value)
      if stmtType == "Declare" then programModel.updateDeclare(Request.UpdateDeclare(stmtId, tpe = Some(newType)))
      else programModel.updateFunction(Request.UpdateFunction(stmtId, tpe = Some(newType)))
    }

    if Statement.hasTpe(stmt, programModel.currentFunction.tpe.toString) then
      typeSelectElem.value = Statement.tpe(stmt, programModel.currentFunction.tpe.toString) // select appropriate type
      stmtElems.appendChild(typeSelectElem)

    // expression input
    val exprInputSize = if stmtType == "ForLoop" then 3 else 10
    val exprInputElem = flowRunElements.newInputText(exprInputSize)
    exprInputElem.value = Statement.expr(stmt)
    exprInputElem.placeholder =
      if stmtType == "Output" then "\"Hello!\""
      else if stmtType == "Call" then "myFun(x)"
      else "x + 1"
    exprInputElem.oninput = _ => {
      import scala.util.*
      val newExprText = exprInputElem.value.trim
      val maybeNewExpr = Try(parseExpr(stmtId, newExprText))
      maybeNewExpr match {
        case Failure(e) =>
          if stmtType == "Declare" && newExprText.isEmpty then
            programModel.updateDeclare(Request.UpdateDeclare(stmtId, expr = Some(None)))
          else if stmtType == "Return" && newExprText.isEmpty then
            programModel.updateReturn(Request.UpdateReturn(stmtId, expr = Some(None)))
          else flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
        case Success(_) =>
          if stmtType == "Declare" then
            programModel.updateDeclare(
              Request.UpdateDeclare(stmtId, expr = Some(Some(newExprText)))
            )
          else if stmtType == "Assign" then
            programModel.updateAssign(Request.UpdateAssign(stmtId, expr = Some(newExprText)))
          else if stmtType == "If" then programModel.updateIf(Request.UpdateIf(stmtId, expr = newExprText))
          else if stmtType == "While" then programModel.updateWhile(Request.UpdateWhile(stmtId, expr = newExprText))
          else if stmtType == "DoWhile" then
            programModel.updateDoWhile(Request.UpdateDoWhile(stmtId, expr = newExprText))
          else if stmtType == "Call" // TODO validate expr is Call()
          then programModel.updateCall(Request.UpdateCall(stmtId, expr = newExprText))
          else if stmtType == "Return" then
            programModel.updateReturn(Request.UpdateReturn(stmtId, expr = Some(Some(newExprText))))
          else if stmtType == "ForLoop" then
            programModel.updateForLoop(Request.UpdateForLoop(stmtId, start = Some(newExprText)))
          else programModel.updateOutput(Request.UpdateOutput(stmtId, newExprText))
      }
    }

    if Statement.hasExpr(stmt) then
      if Set("Declare", "Assign", "ForLoop").contains(stmtType) then {
        stmtElems.appendChild(span(" = ").render)
      }
      stmtElems.appendChild(exprInputElem)
    end if

    // for loop additional inputs
    if stmtType == "ForLoop" then
      val forLoop = stmt.asInstanceOf[Statement.ForLoop]
      val toInputElem = flowRunElements.newInputText(exprInputSize)
      toInputElem.value = forLoop.end
      toInputElem.placeholder = "10"
      toInputElem.oninput = _ => {
        import scala.util.*
        val newExprText = toInputElem.value.trim
        val maybeNewExpr = Try(parseExpr(stmtId, newExprText))
        maybeNewExpr match {
          case Failure(e) =>
            flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
          case Success(_) =>
            programModel.updateForLoop(Request.UpdateForLoop(stmtId, end = Some(newExprText)))
        }
      }

      val byInputElem = flowRunElements.newInputText(exprInputSize)
      byInputElem.value = forLoop.incr
      byInputElem.placeholder = "11"
      byInputElem.oninput = _ => {
        import scala.util.*
        val newExprText = byInputElem.value.trim
        val maybeNewExpr = Try(parseExpr(stmtId, newExprText))
        maybeNewExpr match {
          case Failure(e) =>
            flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
          case Success(_) =>
            programModel.updateForLoop(Request.UpdateForLoop(stmtId, incr = Some(newExprText)))
        }
      }

      stmtElems.appendChild(span(" to ").render)
      stmtElems.appendChild(toInputElem)
      stmtElems.appendChild(span(" by ").render)
      stmtElems.appendChild(byInputElem)
    end if

    flowRunElements.stmtOutput.appendChild(stmtElems)

    // params inputs
    if stmtType == "Begin" then
      val paramsListElem = div().render
      val addParamElem = flowRunElements.addParamButton
      addParamElem.onclick = _ => {
        val newParam =
          Function.Parameter(UUID.randomUUID().toString, s"p${getParams().length + 1}", Expression.Type.Integer)
        val newParams = getParams() ++ List(newParam)
        val paramNameInput = getParamNameInput(stmtId, newParam)
        val paramTpeInput = getParamTpeInput(stmtId, newParam)
        val paramDeleteBtn = getParamDeleteBtn(stmtId, newParam)

        paramsListElem.appendChild(
          div(id := newParam.id, cls := "flowrun-param-inputs")(paramNameInput, paramTpeInput, paramDeleteBtn).render
        )
        paramNameInput.focus()
        programModel.updateFunction(Request.UpdateFunction(stmtId, parameters = Some(newParams)))
      }

      getParams().foreach { param =>
        val paramNameInput = getParamNameInput(stmtId, param)
        val paramTpeInput = getParamTpeInput(stmtId, param)
        val paramDeleteBtn = getParamDeleteBtn(stmtId, param)
        paramsListElem.appendChild(
          div(id := param.id, cls := "flowrun-param-inputs")(paramNameInput, paramTpeInput, paramDeleteBtn).render
        )
      }

      flowRunElements.stmtOutput.appendChild(div(br, small("Parameters:")).render)
      flowRunElements.stmtOutput.appendChild(paramsListElem)
      flowRunElements.stmtOutput.appendChild(div(addParamElem).render)
    end if

    // focus
    if hasName then
      nameInputElem.focus()
      nameInputElem.select()
    else
      exprInputElem.focus()
      val exprStr = exprInputElem.value
      if isQuotedStringLiteral(exprStr) then exprInputElem.setSelectionRange(1, exprStr.length - 1)
  }

  private def getParams(): List[Function.Parameter] =
    programModel.currentFunction.parameters

  private def getParamNameInput(
      stmtId: String,
      param: Function.Parameter
  ) = {
    val paramNameInput = flowRunElements.newInputText()
    paramNameInput.value = param.name
    paramNameInput.oninput = _ => {
      val newName = paramNameInput.value.trim
      val params = getParams()
      val newParams = params.map { p =>
        if p.id == param.id then p.copy(name = newName) else p
      }
      val errorMsg: Option[String] = NameUtils.validateIdentifier(newName)
      if errorMsg.isEmpty then programModel.updateFunction(Request.UpdateFunction(stmtId, parameters = Some(newParams)))
      else flowrunChannel := FlowRun.Event.SyntaxError(errorMsg.get)
    }
    paramNameInput
  }

  private def getParamTpeInput(
      stmtId: String,
      param: Function.Parameter
  ) = {
    val paramTpeInput = flowRunElements.newInputSelect
    Expression.Type.VarTypes.foreach { tpe =>
      val typeItem = option(value := tpe.toString)(tpe.toString).render
      paramTpeInput.add(typeItem)
    }
    paramTpeInput.value = param.tpe.toString
    paramTpeInput.onchange = (e: dom.Event) => {
      val params = getParams()
      val newParams = params.map { p =>
        if p.id == param.id then p.copy(tpe = Expression.Type.valueOf(paramTpeInput.value)) else p
      }
      programModel.updateFunction(Request.UpdateFunction(stmtId, parameters = Some(newParams)))
    }
    paramTpeInput
  }

  private def getParamDeleteBtn(
      stmtId: String,
      param: Function.Parameter
  ) = {
    val paramNameInput = flowRunElements.newDeleteParamButton
    paramNameInput.onclick = _ => {
      val params = getParams()
      val newParams = params.filterNot(_.id == param.id)
      programModel.updateFunction(Request.UpdateFunction(stmtId, parameters = Some(newParams)))
      dom.window.document.getElementById(param.id).remove()
    }
    paramNameInput
  }

  private def isQuotedStringLiteral(str: String) =
    str.length >= 2 && str.head == '"' && str.last == '"'

}
