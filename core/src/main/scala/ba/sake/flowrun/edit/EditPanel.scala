package ba.sake.flowrun
package edit

import scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.*
import reactify.*
import ba.sake.flowrun.cytoscape.*
import ba.sake.flowrun.ProgramModel.Request
import ba.sake.flowrun.parse.*

class EditPanel(programModel: ProgramModel, flowRunElements: FlowRunElements, flowrunChannel: Channel[FlowRun.Event], cy: cytoscape) {

  def setup(): Unit = {
    cy.asDyn.on("select", s"node.${Node.Editable}", (evt: js.Dynamic) => {
      val node = evt.target
      val nodeId = node.data("id").toString
      val nodeType = node.data("tpe").toString
      val varType = node.data("rawTpe").asInstanceOf[js.UndefOr[String]].toOption

      def setLabel(): Unit = {
        val maybeName = node.data("rawName").asInstanceOf[js.UndefOr[String]].toOption.getOrElse("")
        val maybeTpe = node.data("rawTpe").asInstanceOf[js.UndefOr[String]].toOption.getOrElse("")
        val maybeExpr = node.data("rawExpr").asInstanceOf[js.UndefOr[String]].toOption.filterNot(_.trim.isEmpty)
        val maybeParams = node.data("rawParams").asInstanceOf[js.UndefOr[String]].toOption.getOrElse("")
        val maybeExprText = maybeExpr.map(e => s" = $e").getOrElse("")
        val maybeRetExprText = maybeExpr.map(e => s" $e").getOrElse("")
        val (newLabel, mul) = if nodeType == Node.Declare then
          s"""$maybeName: $maybeTpe$maybeExprText""" -> 8
        else if nodeType == Node.Assign then
          s"""$maybeName$maybeExprText""" -> 8
        else if nodeType == Node.Start then
          s"""$maybeName($maybeParams): $maybeTpe""" -> 10
        else if nodeType == Node.Return then
          s"""return$maybeRetExprText""" -> 10
        else if nodeType == Node.Input then
          s"""$maybeName""" -> 8
        else maybeExpr.getOrElse("") -> 10
        val newLabelLength = 65 max (newLabel.length * mul)
        node.data("label", newLabel)
        node.data("width", newLabelLength)
      }
      
      val nameInputElem = flowRunElements.newInputText
      nameInputElem.value = node.data("rawName").asInstanceOf[js.UndefOr[String]].getOrElse("")
      nameInputElem.placeholder = if nodeType == Node.Start then "myFun" else "x"
      nameInputElem.oninput = (_: dom.Event) => {
        val newName = nameInputElem.value.trim
        val errorMsg = if newName.isEmpty then None // noop when blank
          else if !newName.head.isLetter then Some("Name must start with a letter.")
          else if newName.matches(".*\\s.*") then Some("Name must not contain spaces.")
          else if !newName.matches("[a-zA-Z0-9_]+") then Some("Name can contain only letters, numbers and underscore.")
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
            setLabel()
            None
        
        errorMsg match
          case Some(msg) =>
            flowrunChannel := FlowRun.Event.SyntaxError(msg)
          case None =>
            flowrunChannel := FlowRun.Event.SyntaxSuccess
      }
      
      val exprInputElem = flowRunElements.newInputText
      exprInputElem.value = node.data("rawExpr").asInstanceOf[js.UndefOr[String]].getOrElse("")
      exprInputElem.placeholder = if nodeType == Node.Output then "\"Hello!\""
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
              setLabel()
            else if nodeType == Node.Return && newExprText.isEmpty then
              programModel.updateReturn(Request.UpdateReturn(nodeId, expr = Some(None)))
              node.data("rawExpr", newExprText)
              setLabel()
            else
              flowrunChannel := FlowRun.Event.SyntaxError(e.getMessage)
          case Success(_) =>
            flowrunChannel := FlowRun.Event.SyntaxSuccess
            if nodeType == Node.Declare then
              programModel.updateDeclare(Request.UpdateDeclare(nodeId, expr = Some(Some(newExprText))))
            else if nodeType == Node.Assign then
              programModel.updateAssign(Request.UpdateAssign(nodeId, expr = Some(newExprText)))
            else if nodeType == Node.If then
              programModel.updateIf(Request.UpdateIf(nodeId, expr = newExprText))
            else if nodeType == Node.Call then
              programModel.updateCall(Request.UpdateCall(nodeId, expr = newExprText))
            else if nodeType == Node.Return then
              programModel.updateReturn(Request.UpdateReturn(nodeId, expr = Some(Some(newExprText))))
            else
              programModel.updateOutput(Request.UpdateOutput(nodeId, newExprText))

            node.data("rawExpr", newExprText)
            setLabel()
        }
      }

      val typeSelectElem = flowRunElements.newInputSelect
      Expression.Type.values.foreach { tpe =>
        val typeItem = option(value := tpe.toString)(tpe.toString).render
        typeSelectElem.add(typeItem)
      }
      typeSelectElem.onchange = { (e: dom.Event) =>
        val thisElem = e.target.asInstanceOf[dom.html.Select]
        val newType = Expression.Type.values(thisElem.selectedIndex)
        if nodeType == Node.Declare then
          programModel.updateDeclare(Request.UpdateDeclare(nodeId, tpe = Some(newType)))
        else
          programModel.updateFunction(Request.UpdateFunction(nodeId, tpe = Some(newType)))
        
        node.data("rawTpe", newType.toString)
        setLabel()
      }

      // clear first, prepare for new inputs
      flowRunElements.editStatement.innerText = ""
      flowRunElements.editStatement.appendChild(div(s"Editing $nodeType:").render)

      // append edit elements
      var hasName = false
      var filledName = false
      if (Set(Node.Declare, Node.Start, Node.Assign, Node.Input).contains(nodeType)) {
        hasName = true
        filledName = nameInputElem.value.nonEmpty
        flowRunElements.editStatement.appendChild(nameInputElem)
      }

      if (Set(Node.Declare, Node.Start).contains(nodeType)) {
        typeSelectElem.value = varType.get // select appropriate type
        flowRunElements.editStatement.appendChild(span(": ").render)
        flowRunElements.editStatement.appendChild(typeSelectElem)
      }

      var hasExpr = false
      if (Set(Node.Declare, Node.Assign, Node.Output, Node.Call, Node.Return, Node.If).contains(nodeType)) {
        hasExpr = true
        if Set(Node.Declare, Node.Assign).contains(nodeType) then
          flowRunElements.editStatement.appendChild(span(" = ").render)
        flowRunElements.editStatement.appendChild(exprInputElem)
      }

      if !hasExpr || (hasName && !filledName) then
        nameInputElem.focus()
        nameInputElem.select()
      else
        exprInputElem.focus()
        val exprStr = exprInputElem.value
        if exprStr.length > 2 && exprStr.count(_ == '"') == 2 && exprStr.head == '"' && exprStr.last == '"' then
          exprInputElem.setSelectionRange(1, exprStr.length - 1)
    })    
  }
}