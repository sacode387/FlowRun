package dev.sacode.flowrun
package edit

import scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.*
import reactify.*
import dev.sacode.flowrun.cytoscape.*
import dev.sacode.flowrun.ProgramModel.Request
import dev.sacode.flowrun.parse.*

class EditPanel(
    programModel: ProgramModel,
    flowRunElements: FlowRunElements,
    flowrunChannel: Channel[FlowRun.Event],
    cy: cytoscape
) {

  def setup(): Unit = {
    cy.asDyn.on(
      "select",
      s"node.${Node.Editable}",
      (evt: js.Dynamic) => {
        val node = evt.target
        val nodeId = node.data("id").toString
        val nodeType = node.data("tpe").toString
        val varType = node.data("rawTpe").asInstanceOf[js.UndefOr[String]].toOption

        // clear first, prepare for new inputs
        flowRunElements.editStatement.innerText = ""
        flowRunElements.editStatement.appendChild(div(s"Editing $nodeType:").render)

        // name input
        val nameInputElem = flowRunElements.newInputText
        nameInputElem.value = node.data("rawName").asInstanceOf[js.UndefOr[String]].getOrElse("")
        nameInputElem.placeholder = if nodeType == Node.Start then "myFun" else "x"
        nameInputElem.oninput = (_: dom.Event) => {
          val newName = nameInputElem.value.trim
          val errorMsg =
            if newName.isEmpty then None // noop when blank
            else if !newName.head.isLetter then Some("Name must start with a letter.")
            else if newName.matches(".*\\s.*") then Some("Name must not contain spaces.")
            else if !newName.matches("[a-zA-Z0-9_]+") then
              Some("Name can contain only letters, numbers and underscore.")
            else
              if nodeType == Node.Declare then
                programModel.updateDeclare(Request.UpdateDeclare(nodeId, name = Some(newName)))
              else if nodeType == Node.Input then
                programModel.updateInput(Request.UpdateInput(nodeId, name = newName))
              else if nodeType == Node.Start then
                programModel.updateFunction(Request.UpdateFunction(nodeId, name = Some(newName)))
                flowrunChannel := FlowRun.Event.FunctionUpdated
              else if nodeType == Node.Assign then
                programModel.updateAssign(Request.UpdateAssign(nodeId, name = Some(newName)))
              node.data("rawName", newName)
              setLabel(node, nodeType)
              None

          errorMsg match
            case Some(msg) =>
              flowrunChannel := FlowRun.Event.SyntaxError(msg)
            case None =>
              flowrunChannel := FlowRun.Event.SyntaxSuccess
        }

        var hasName = false
        var filledName = false
        if Set(Node.Declare, Node.Start, Node.Assign, Node.Input).contains(nodeType) then
          hasName = true
          filledName = nameInputElem.value.nonEmpty
          flowRunElements.editStatement.appendChild(nameInputElem)

        // type input
        val typeSelectElem = flowRunElements.newInputSelect
        val types =
          if nodeType == Node.Start then Expression.Type.values
          else Expression.Type.VarTypes
        types.foreach { tpe =>
          val typeItem = option(value := tpe.toString)(tpe.toString).render
          typeSelectElem.add(typeItem)
        }
        typeSelectElem.onchange = { (e: dom.Event) =>
          val thisElem = e.target.asInstanceOf[dom.html.Select]
          val newType = Expression.Type.valueOf(thisElem.value)
          if nodeType == Node.Declare then
            programModel.updateDeclare(Request.UpdateDeclare(nodeId, tpe = Some(newType)))
          else programModel.updateFunction(Request.UpdateFunction(nodeId, tpe = Some(newType)))

          node.data("rawTpe", newType.toString)
          setLabel(node, nodeType)
        }

        if Set(Node.Declare, Node.Start).contains(nodeType) then
          typeSelectElem.value = varType.get // select appropriate type
          flowRunElements.editStatement.appendChild(span(": ").render)
          flowRunElements.editStatement.appendChild(typeSelectElem)

        // expression input
        val exprInputElem = flowRunElements.newInputText
        exprInputElem.value = node.data("rawExpr").asInstanceOf[js.UndefOr[String]].getOrElse("")
        exprInputElem.placeholder =
          if nodeType == Node.Output then "\"Hello!\""
          else if nodeType == Node.Call then "myFun(x)"
          else "x + 1"
        exprInputElem.oninput = _ => {
          import scala.util.*
          val newExprText = exprInputElem.value.trim
          val maybeNewExpr = Try(parseExpr(nodeId, newExprText))
          maybeNewExpr match {
            case Failure(e) =>
              if nodeType == Node.Declare && newExprText.isEmpty then
                programModel.updateDeclare(Request.UpdateDeclare(nodeId, expr = Some(None)))
                node.data("rawExpr", newExprText)
                setLabel(node, nodeType)
              else if nodeType == Node.Return && newExprText.isEmpty then
                programModel.updateReturn(Request.UpdateReturn(nodeId, expr = Some(None)))
                node.data("rawExpr", newExprText)
                setLabel(node, nodeType)
              else flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
            case Success(_) =>
              flowrunChannel := FlowRun.Event.SyntaxSuccess
              if nodeType == Node.Declare then
                programModel.updateDeclare(
                  Request.UpdateDeclare(nodeId, expr = Some(Some(newExprText)))
                )
              else if nodeType == Node.Assign then
                programModel.updateAssign(Request.UpdateAssign(nodeId, expr = Some(newExprText)))
              else if nodeType == Node.If then
                programModel.updateIf(Request.UpdateIf(nodeId, expr = newExprText))
              else if nodeType == Node.While then
                programModel.updateWhile(Request.UpdateWhile(nodeId, expr = newExprText))
              else if nodeType == Node.DoWhile then
                programModel.updateDoWhile(Request.UpdateDoWhile(nodeId, expr = newExprText))
              else if nodeType == Node.Call then
                programModel.updateCall(Request.UpdateCall(nodeId, expr = newExprText))
              else if nodeType == Node.Return then
                programModel.updateReturn(
                  Request.UpdateReturn(nodeId, expr = Some(Some(newExprText)))
                )
              else programModel.updateOutput(Request.UpdateOutput(nodeId, newExprText))

              node.data("rawExpr", newExprText)
              setLabel(node, nodeType)
          }
        }

        var hasExpr = false
        if Set(Node.Declare, Node.Assign, Node.Output, Node.Call, Node.Return, Node.If, Node.While, Node.DoWhile).contains(
            nodeType
          )
        then
          hasExpr = true
          if Set(Node.Declare, Node.Assign).contains(nodeType) then
            flowRunElements.editStatement.appendChild(span(" = ").render)
          flowRunElements.editStatement.appendChild(exprInputElem)

        // params inputs
        if nodeType == Node.Start then
          val addParamElem = flowRunElements.addParamButton
          addParamElem.onclick = _ => {
            val name = ""
            val tpe = Expression.Type.Integer.toString
            val params = getParams(node)
            val idx = params.length
            val newParams = params ++ List(name -> tpe)
            val paramNameInput = getParamNameInput(node, nodeType, nodeId, name, idx)
            val paramTpeInput = getParamTpeInput(node, nodeType, nodeId, tpe, idx)
            flowRunElements.editStatement.appendChild(
              div(paramNameInput, paramTpeInput).render
            )
            paramNameInput.focus()
            programModel.updateFunction(
              Request.UpdateFunction(nodeId, parameters = Some(newParams))
            )
            node.data("rawParams", newParams.map((n, t) => s"$n: $t").mkString(","))
            setLabel(node, nodeType)
          }
          flowRunElements.editStatement.appendChild(div(addParamElem).render)

          val params = getParams(node)
          params.zipWithIndex.foreach { case ((name, tpe), idx) =>
            val paramNameInput = getParamNameInput(node, nodeType, nodeId, name, idx)
            val paramTpeInput = getParamTpeInput(node, nodeType, nodeId, tpe, idx)
            flowRunElements.editStatement.appendChild(
              div(paramNameInput, paramTpeInput).render
            )
          }
        end if

        // focus
        if !hasExpr || (hasName && !filledName) then
          nameInputElem.focus()
          nameInputElem.select()
        else
          exprInputElem.focus()
          val exprStr = exprInputElem.value
          if exprStr.length > 2 && exprStr.count(
              _ == '"'
            ) == 2 && exprStr.head == '"' && exprStr.last == '"'
          then exprInputElem.setSelectionRange(1, exprStr.length - 1)
      }
    )
  }

  private def setLabel(node: js.Dynamic, nodeType: String): Unit = {
    val maybeName = node.data("rawName").asInstanceOf[js.UndefOr[String]].toOption.getOrElse("")
    val maybeTpe = node.data("rawTpe").asInstanceOf[js.UndefOr[String]].toOption.getOrElse("")
    val maybeExpr =
      node.data("rawExpr").asInstanceOf[js.UndefOr[String]].toOption.filterNot(_.trim.isEmpty)
    val params = node.data("rawParams").asInstanceOf[js.UndefOr[String]].toOption.getOrElse("")
    val maybeExprText = maybeExpr.map(e => s" = $e").getOrElse("")
    val maybeRetExprText = maybeExpr.map(e => s" $e").getOrElse("")
    val (newLabel, mul) =
      if nodeType == Node.Declare then s"""$maybeName: $maybeTpe$maybeExprText""" -> 8
      else if nodeType == Node.Assign then s"""$maybeName$maybeExprText""" -> 8
      else if nodeType == Node.Start then s"""$maybeName($params): $maybeTpe""" -> 10
      else if nodeType == Node.Return then s"""return$maybeRetExprText""" -> 10
      else if nodeType == Node.Input then s"""$maybeName""" -> 8
      else maybeExpr.getOrElse("") -> 10
    val newLabelLength = 65 max (newLabel.length * mul)
    node.data("label", newLabel)
    node.data("width", newLabelLength)
  }

  private def getParams(node: js.Dynamic): List[(String, String)] = {
    val paramsStr = node.data("rawParams").asInstanceOf[js.UndefOr[String]].toOption.getOrElse("")
    if paramsStr.trim.isEmpty then List.empty
    else
      paramsStr
        .split(",")
        .map { p =>
          val parts = p.split(":")
          parts(0).trim -> parts(1).trim
        }
        .toList
  }

  private def getParamNameInput(
      node: js.Dynamic,
      nodeType: String,
      nodeId: String,
      name: String,
      idx: Int
  ) = {
    val paramNameInput = flowRunElements.newInputText
    paramNameInput.value = name
    paramNameInput.oninput = _ => {
      val params = getParams(node)
      val newParam = params(idx).copy(_1 = paramNameInput.value)
      val newParams = params.patch(idx, List(newParam), 1)
      programModel.updateFunction(Request.UpdateFunction(nodeId, parameters = Some(newParams)))
      node.data("rawParams", newParams.map((n, t) => s"$n: $t").mkString(","))
      setLabel(node, nodeType)
    }
    paramNameInput
  }

  private def getParamTpeInput(
      node: js.Dynamic,
      nodeType: String,
      nodeId: String,
      selValue: String,
      idx: Int
  ) = {
    val paramTpeInput = flowRunElements.newInputSelect
    Expression.Type.VarTypes.foreach { tpe =>
      val typeItem = option(value := tpe.toString)(tpe.toString).render
      paramTpeInput.add(typeItem)
    }
    paramTpeInput.value = selValue
    paramTpeInput.onchange = (e: dom.Event) => {
      val params = getParams(node)
      val newParam = params(idx).copy(_2 = paramTpeInput.value)
      val newParams = params.patch(idx, List(newParam), 1)
      programModel.updateFunction(Request.UpdateFunction(nodeId, parameters = Some(newParams)))
      node.data("rawParams", newParams.map((n, t) => s"$n: $t").mkString(","))
      setLabel(node, nodeType)
    }
    paramTpeInput
  }
}
