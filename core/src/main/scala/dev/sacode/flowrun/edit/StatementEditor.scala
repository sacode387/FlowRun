package dev.sacode.flowrun
package edit

import scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.{name => _, *}
import reactify.*
import dev.sacode.flowrun.ProgramModel.Request
import dev.sacode.flowrun.parse.*
import java.util.UUID

/** Editor for selected statement. */
class StatementEditor(
    programModel: ProgramModel,
    flowrunChannel: Channel[FlowRun.Event],
    flowRunElements: FlowRunElements
) {

  private val EditableNodeTypes =
    Set("Begin", "Return", "Declare", "Assign", "Input", "Output", "Call", "If", "While", "DoWhile")

  def edit(nodeId: String, nodeTpe: String): Unit = {
    if !EditableNodeTypes(nodeTpe) then return

    val node = programModel.findStatement(nodeId)
    val nodeType = node.getClass.getSimpleName.filterNot(_ == '$')

    // skip Begin if main function
    if nodeType == "Begin" && programModel.currentFunction.isMain then return

    // skip Return if function doesn't return anything
    if nodeType == "Return" && programModel.currentFunction.tpe == Expression.Type.Void then return

    // clear first, prepare for new inputs
    flowRunElements.stmtOutput.innerText = ""
    flowRunElements.stmtOutput.appendChild(div(s"Editing $nodeType:").render)

    // name input
    val nameInputElem = flowRunElements.newInputText
    nameInputElem.value = Statement.name(node, programModel.currentFunction.name)
    nameInputElem.placeholder = if nodeType == "Begin" then "myFun" else "x"
    nameInputElem.oninput = { (_: dom.Event) =>
      val newName = nameInputElem.value.trim
      val errorMsg: Option[String] = NameUtils.validateIdentifier(newName)
      if errorMsg.isEmpty then {
        node match {
          case _: Statement.Declare =>
            programModel.updateDeclare(Request.UpdateDeclare(nodeId, name = Some(newName)))
          case _: Statement.Input =>
            programModel.updateInput(Request.UpdateInput(nodeId, name = newName))
          case _: Statement.Begin =>
            programModel.updateFunction(Request.UpdateFunction(nodeId, name = Some(newName)))
          case _: Statement.Assign =>
            programModel.updateAssign(Request.UpdateAssign(nodeId, name = Some(newName)))
          case _ => ()
        }
      } else {
        flowrunChannel := FlowRun.Event.SyntaxError(errorMsg.get)
      }
    }

    var filledName = false
    if Statement.hasName(node, programModel.currentFunction.name) then
      filledName = nameInputElem.value.nonEmpty
      flowRunElements.stmtOutput.appendChild(nameInputElem)

    // type input
    val typeSelectElem = flowRunElements.newInputSelect
    val types =
      if nodeType == "Begin" then Expression.Type.values
      else Expression.Type.VarTypes
    types.foreach { tpe =>
      val typeItem = option(value := tpe.toString)(tpe.toString).render
      typeSelectElem.add(typeItem)
    }
    typeSelectElem.onchange = { (e: dom.Event) =>
      val thisElem = e.target.asInstanceOf[dom.html.Select]
      val newType = Expression.Type.valueOf(thisElem.value)
      if nodeType == "Declare" then programModel.updateDeclare(Request.UpdateDeclare(nodeId, tpe = Some(newType)))
      else programModel.updateFunction(Request.UpdateFunction(nodeId, tpe = Some(newType)))
    }

    if Statement.hasTpe(node, programModel.currentFunction.tpe.toString) then
      typeSelectElem.value = Statement.tpe(node, programModel.currentFunction.tpe.toString) // select appropriate type
      flowRunElements.stmtOutput.appendChild(span(": ").render)
      flowRunElements.stmtOutput.appendChild(typeSelectElem)

    // expression input
    val exprInputElem = flowRunElements.newInputText
    exprInputElem.value = Statement.expr(node)
    exprInputElem.placeholder =
      if nodeType == "Output" then "\"Hello!\""
      else if nodeType == "Call" then "myFun(x)"
      else "x + 1"
    exprInputElem.oninput = _ => {
      import scala.util.*
      val newExprText = exprInputElem.value.trim
      val maybeNewExpr = Try(parseExpr(nodeId, newExprText))
      maybeNewExpr match {
        case Failure(e) =>
          if nodeType == "Declare" && newExprText.isEmpty then
            programModel.updateDeclare(Request.UpdateDeclare(nodeId, expr = Some(None)))
          else if nodeType == "Return" && newExprText.isEmpty then
            programModel.updateReturn(Request.UpdateReturn(nodeId, expr = Some(None)))
          else flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
        case Success(_) =>
          if nodeType == "Declare" then
            programModel.updateDeclare(
              Request.UpdateDeclare(nodeId, expr = Some(Some(newExprText)))
            )
          else if nodeType == "Assign" then
            programModel.updateAssign(Request.UpdateAssign(nodeId, expr = Some(newExprText)))
          else if nodeType == "If" then programModel.updateIf(Request.UpdateIf(nodeId, expr = newExprText))
          else if nodeType == "While" then programModel.updateWhile(Request.UpdateWhile(nodeId, expr = newExprText))
          else if nodeType == "DoWhile" then
            programModel.updateDoWhile(Request.UpdateDoWhile(nodeId, expr = newExprText))
          else if nodeType == "Call" // TODO validate expr is Call()
          then programModel.updateCall(Request.UpdateCall(nodeId, expr = newExprText))
          else if nodeType == "Return" then
            programModel.updateReturn(
              Request.UpdateReturn(nodeId, expr = Some(Some(newExprText)))
            )
          else programModel.updateOutput(Request.UpdateOutput(nodeId, newExprText))
      }
    }

    if Statement.hasExpr(node) then
      if Set("Declare", "Assign").contains(nodeType) then flowRunElements.stmtOutput.appendChild(span(" = ").render)
      flowRunElements.stmtOutput.appendChild(exprInputElem)
    end if

    // params inputs
    if nodeType == "Begin" then

      val addParamElem = flowRunElements.addParamButton
      addParamElem.onclick = _ => {
        val newParam =
          Function.Parameter(UUID.randomUUID().toString, s"p${getParams().length + 1}", Expression.Type.Integer)
        val newParams = getParams() ++ List(newParam)
        val paramNameInput = getParamNameInput(nodeId, newParam)
        val paramTpeInput = getParamTpeInput(nodeId, newParam)
        val paramDeleteBtn = getParamDeleteBtn(nodeId, newParam)

        flowRunElements.stmtOutput.appendChild(
          div(id := newParam.id)(paramNameInput, paramTpeInput, paramDeleteBtn).render
        )
        paramNameInput.focus()
        programModel.updateFunction(Request.UpdateFunction(nodeId, parameters = Some(newParams)))
      }
      flowRunElements.stmtOutput.appendChild(div(addParamElem).render)

      getParams().foreach { param =>
        val paramNameInput = getParamNameInput(nodeId, param)
        val paramTpeInput = getParamTpeInput(nodeId, param)
        val paramDeleteBtn = getParamDeleteBtn(nodeId, param)
        flowRunElements.stmtOutput.appendChild(
          div(id := param.id)(paramNameInput, paramTpeInput, paramDeleteBtn).render
        )
      }
    end if

    // focus
    if !Statement.hasExpr(node) || (Statement.hasName(
        node,
        programModel.currentFunction.name
      ) && !filledName)
    then
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
      nodeId: String,
      param: Function.Parameter
  ) = {
    val paramNameInput = flowRunElements.newInputText
    paramNameInput.value = param.name
    paramNameInput.oninput = _ => {
      val params = getParams()
      val newParams = params.map { p =>
        if p.id == param.id then p.copy(name = paramNameInput.value) else p
      }
      val errorMsg: Option[String] = NameUtils.validateIdentifier(newName)
      if errorMsg.isEmpty then programModel.updateFunction(Request.UpdateFunction(nodeId, parameters = Some(newParams)))
      else flowrunChannel := FlowRun.Event.SyntaxError(errorMsg.get)
    }
    paramNameInput
  }

  private def getParamTpeInput(
      nodeId: String,
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
      programModel.updateFunction(Request.UpdateFunction(nodeId, parameters = Some(newParams)))
    }
    paramTpeInput
  }

  private def getParamDeleteBtn(
      nodeId: String,
      param: Function.Parameter
  ) = {
    val paramNameInput = flowRunElements.newDeleteParamButton
    paramNameInput.onclick = _ => {
      val params = getParams()
      val newParams = params.filterNot(_.id == param.id)
      programModel.updateFunction(Request.UpdateFunction(nodeId, parameters = Some(newParams)))
      dom.window.document.getElementById(param.id).remove()
    }
    paramNameInput
  }

  private def isQuotedStringLiteral(str: String) =
    str.length >= 2 && str.head == '"' && str.last == '"'

}
