package ba.sake.flowrun
package cytoscape

import scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all.*

import ba.sake.flowrun.parse.*
import ba.sake.flowrun.ProgramModel.Request

class CytoscapeFlowchart(
  programModel: ProgramModel,
  container: dom.Element,
  editWrapperElem: dom.Element
) {
  
  val cy = new cytoscape(
    js.Dynamic.literal(
      container = container,
      userZoomingEnabled = false,
      userPanningEnabled = false,
      boxSelectionEnabled = false,
      maxZoom = 1.3,
      autoungrabify = true,
      style = utils.styleJson
    )
  )

  loadCurrentFunction()
  setupMenus()
  setupEditPanel()
  

  cy.asDyn.on("unselect add remove", "node", (evt: js.Dynamic) => {
    // node not selected anymore, hide the inputs...
    editWrapperElem.innerText = ""
    cy.asDyn.nodes().unselect()
    // clear visual error on nodes
    clearErrors()
  })

  dom.document.addEventListener("eval-error", (e: dom.CustomEvent) => {
    val nodeId = e.detail.asDyn.nodeId
    cy.asDyn.nodes(s"node[id = '$nodeId']").data("has-error", true)
  })

  def clearErrors(): Unit =
    cy.asDyn.nodes().data("has-error", false)
    EventUtils.dispatchEvent("syntax-success", null)

  def loadCurrentFunction(): Unit = {
    //println("BEFORE: " + js.JSON.stringify(cy.asDyn.elements().jsons()))
    cy.remove("*")
    val statements = programModel.currentFunction.statements
    val firstStmt = statements.head
    val firstNode = Node(firstStmt.label, Node.Begin, id = firstStmt.id)
    cy.add(firstNode.toLit)
    val firstEdge = cy.add(Edge(firstNode.id, firstNode.id).toLit)
    load(statements.tail, firstNode, firstEdge)
    doLayout()
  }
  
  def load(statements: List[Statement], lastNode: Node, lastEdge: js.Dynamic): (Node, js.Dynamic) = {
    import Statement._

    var prevNode = lastNode
    var prevEdge = lastEdge

    statements.foreach {
      case stmt: Input =>
        val newNode = Node(stmt.label, Node.Input, id = stmt.id, rawName = stmt.name)
        cy.add(newNode.toLit)
        cy.add(Edge(prevNode.id, newNode.id).toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
        prevNode = newNode
        prevEdge = cy.add(Edge(newNode.id, newNode.id).toLit)
      case stmt: Output =>
        val newNode = Node(stmt.label, Node.Output, id = stmt.id, rawExpr = stmt.value)
        cy.add(newNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
        prevNode = newNode
        prevEdge = cy.add(Edge(newNode.id, newNode.id).toLit)
      case stmt: Declare =>
        val newNode = Node(stmt.label, Node.Declare, id = stmt.id, rawName = stmt.name, rawTpe = stmt.tpe.toString)
        cy.add(newNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
        prevNode = newNode
        prevEdge = cy.add(Edge(newNode.id, newNode.id).toLit)
      case stmt: Assign =>
        val newNode = Node(stmt.label, Node.Assign, id = stmt.id, rawName = stmt.name, rawExpr = stmt.value)
        cy.add(newNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
        prevNode = newNode
        prevEdge = cy.add(Edge(newNode.id, newNode.id).toLit)
      case Block(_, blockStats) =>
        println("TODO")
      case stmt @ If(id, expr, trueBlock, falseBlock) =>
        val ifEndNode = Node("", Node.IfEnd)
        val ifNode = Node(stmt.condition, Node.If, endId = ifEndNode.id)
        cy.add(ifNode.toLit)
        cy.add(ifEndNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = ifNode.id))
        val trueEdge = cy.add(Edge(ifNode.id, ifNode.id, "true").toLit)
        val falseEdge = cy.add(Edge(ifNode.id, ifEndNode.id, "false").toLit)
        
        val lastTrueEdge = if trueBlock.statements.isEmpty then
          val trueNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)
          cy.add(trueNode.toLit)
          trueEdge.move(js.Dynamic.literal(target = trueNode.id))
          cy.add(Edge(trueNode.id, trueNode.id).toLit)
        else
          val res = load(trueBlock.statements, ifNode, trueEdge)
          //prevNode = res._1
          res._2
        
        val lastFalseEdge = if falseBlock.statements.isEmpty then
          val falseNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)
          cy.add(falseNode.toLit)
          falseEdge.move(js.Dynamic.literal(target = falseNode.id))
          cy.add(Edge(falseNode.id, falseNode.id).toLit)
        else
          val res = load(falseBlock.statements, ifNode, falseEdge)
          //prevNode = res._1
          res._2

        lastTrueEdge.move(js.Dynamic.literal(target = ifEndNode.id))
        lastFalseEdge.move(js.Dynamic.literal(target = ifEndNode.id))

        prevEdge = cy.add(Edge(ifEndNode.id, ifEndNode.id, dir = "vert", blockId = trueBlock.id).toLit)
        prevNode = ifEndNode

      case stmt @ (Begin | End | _: Dummy | _: BlockEnd) =>
        val nodeType = stmt.getClass.getSimpleName.reverse.dropWhile(_ == '$').reverse
        println("tpe: " + nodeType)
        val newNode = Node(stmt.label, nodeType, id = stmt.id)
        cy.add(newNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
        prevNode = newNode
       // prevEdge = cy.add(Edge(newNode.id, newNode.id).toLit)
    }
    (prevNode, prevEdge)
  }

  private def setupMenus(): Unit = {
    cy.contextMenus(
      js.Dynamic.literal(
        evtType = "cxttap", // right-click
        menuItems = js.Array(
          js.Dynamic.literal(
            id = "remove",
            content = "remove",
            tooltipText = "Remove statement",
            image = js.Dynamic.literal(src = "images/delete.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"node.${Node.If}, node.${Node.Input}, node.${Node.Output}, node.${Node.Declare}, node.${Node.Assign}",
            onClickFunction = { (event: dom.Event) =>
              val target = event.target.asDyn

              val prevEdge = target.incomers("edge").first()
              val prevEdgeLabel = prevEdge.data("label").toString
              val prevId = target.incomers("node").first().data("id").toString

              // MAYBE END NODE (IF/WHILE/FOR)
              val maybeEndId = Option(target.data("endId").toString).filterNot(_.trim.isEmpty)
              val nextId = maybeEndId match
                case Some(endId) =>
                  cy.asDyn.nodes(s"node[id = '$endId']").outgoers("node").first().data("id").toString
                case None =>
                  target.outgoers("node").first().data("id").toString
              
              val isNextNodeEnd = target.outgoers("node").first().data("tpe").toString == Node.IfEnd

              if (prevEdgeLabel == "true" || prevEdgeLabel == "false") && isNextNodeEnd then
                val dummyNode = Node("", Node.Dummy, startId = prevId, endId = nextId)
                cy.add(dummyNode.toLit)
                cy.add(Edge(dummyNode.id, nextId, dir = "vert").toLit)
                prevEdge.move(js.Dynamic.literal(target = dummyNode.id)) // rebind
              else 
                prevEdge.move(js.Dynamic.literal(target = nextId)) // rebind

              val toDeleteIds = getDeleteIds(target, maybeEndId)
              toDeleteIds.foreach(id => cy.remove(s"node[id = '$id']"))
              
              doLayout()

              programModel.delete(Request.Delete(target.data("id").toString))
            }
          ),
          js.Dynamic.literal(
            id = "add-output",
            content = "output",
            tooltipText = "Add output statement",
            image = js.Dynamic.literal(src = "images/output.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("\"output\"", Node.Output, rawExpr = "\"output\"")
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))
              
              doLayout()

              programModel.addOutput(Request.AddOutput(newNode.id, edge.source().data("id").toString, edge.data("blockId").toString))
            }
          ),
          js.Dynamic.literal(
            id = "add-input",
            content = "input",
            tooltipText = "Add input statement",
            image = js.Dynamic.literal(src = "images/input.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("input", Node.Input, rawName = "input")
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))
              
              doLayout()

              programModel.addInput(Request.AddInput(newNode.id, edge.source().data("id").toString, edge.data("blockId").toString))
            },
            hasTrailingDivider = true
          ),
          js.Dynamic.literal(
            id = "add-declare",
            content = "declare",
            tooltipText = "Add declare statement",
            image = js.Dynamic.literal(src = "images/declare.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("declare", Node.Declare, rawTpe = "Integer")
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout()

              programModel.addDeclare(Request.AddDeclare(newNode.id, "", Expression.Type.Integer, edge.source().data("id").toString, edge.data("blockId").toString))
            }
          ),
          js.Dynamic.literal(
            id = "add-assign",
            content = "assign",
            tooltipText = "Add assign statement",
            image = js.Dynamic.literal(src = "images/assign.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("assign", Node.Assign)
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout()

              programModel.addAssign(Request.AddAssign(newNode.id, edge.source().data("id").toString, edge.data("blockId").toString))
            },
            hasTrailingDivider = true
          ),
          js.Dynamic.literal(
            id = "add-if",
            content = "if",
            tooltipText = "Add if statement",
            image = js.Dynamic.literal(src = "images/if.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)

              val ifEndNode = Node("", Node.IfEnd)
              val ifNode = Node("if", Node.If, endId = ifEndNode.id)
              val trueNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)
              val falseNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)
              
              cy.add(ifNode.toLit)
              cy.add(trueNode.toLit)
              cy.add(falseNode.toLit)
              cy.add(ifEndNode.toLit)
              edge.move(js.Dynamic.literal(target = ifNode.id))
              val falseEdge = Edge(ifNode.id, falseNode.id, "false")
              val trueEdge = Edge(ifNode.id, trueNode.id, "true")
              
              cy.add(trueEdge.toLit)
              cy.add(falseEdge.toLit)
              
              cy.add(Edge(falseNode.id, ifEndNode.id, dir = "vert").toLit)
              cy.add(Edge(trueNode.id, ifEndNode.id, dir = "vert").toLit)
              cy.add(Edge(ifEndNode.id, nextNodeId, dir = "vert", blockId = edge.data("blockId").toString).toLit)

              /*
              val ifNode = Node("if", Node.If)
              val ifEndNode = Node("", Node.IfEnd)
              cy.add(ifNode.toLit)
              cy.add(ifEndNode.toLit)
              edge.move(js.Dynamic.literal(target = ifNode.id))
              cy.add(Edge(ifNode.id, ifEndNode.id, "true", dir = "vert").toLit)
              cy.add(Edge(ifNode.id, ifEndNode.id, "false", dir = "vert").toLit)
              cy.add(Edge(ifEndNode.id, nextNodeId).toLit)
              */

              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))
              
              doLayout()

              programModel.addIf(Request.AddIf(ifNode.id, trueEdge.id, falseEdge.id, ifEndNode.id, edge.source().data("id").toString, edge.data("blockId").toString))
            }
          )
        )
      )
    )
  }

  private def setupEditPanel(): Unit = {   
    cy.asDyn.on("select", s"node.${Node.Editable}", (evt: js.Dynamic) => {
      val node = evt.target
      val nodeId = node.data("id").toString
      val nodeType = node.data("tpe").toString
      val varType = node.data("rawTpe").asInstanceOf[js.UndefOr[String]].toOption

      def setLabel(): Unit = {
        val maybeName = node.data("rawName").asInstanceOf[js.UndefOr[String]].toOption
        val maybeExpr = node.data("rawExpr").asInstanceOf[js.UndefOr[String]].toOption.filterNot(_.trim.isEmpty)
        val maybeExprText = maybeExpr.map(e => s" = $e").getOrElse("")
        val (newLabel, mul) = if nodeType == Node.Declare then
          s"""${maybeName.get}: ${node.data("rawTpe")}$maybeExprText""" -> 8
        else if nodeType == Node.Assign then
          s"""${maybeName.get}$maybeExprText""" -> 8
        else if nodeType == Node.Input then
          s"""${maybeName.get}""" -> 8
        else maybeExpr.getOrElse("") -> 10
        val newLabelLength = 65 max (newLabel.length * mul)
        node.data("label", newLabel)
        node.data("width", newLabelLength)
      }
      
      val nameInputElem = input().render
      val nameLabelElem = label(
        "Name: ",
        nameInputElem
      ).render
      nameInputElem.value = node.data("rawName").asInstanceOf[js.UndefOr[String]].getOrElse("")
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
            else
              programModel.updateAssign(Request.UpdateAssign(nodeId, name = Some(newName)))
            node.data("rawName", newName)
            setLabel()
            None
        
        errorMsg match
          case Some(msg) =>
            EventUtils.dispatchEvent("syntax-error", js.Dynamic.literal(msg = msg))
          case None =>
            EventUtils.dispatchEvent("syntax-success", null)
      }
      
      val exprInputElem = input().render
      val exprLabelElem = label(
        "Expression: ",
        exprInputElem
      ).render
      exprInputElem.value = node.data("rawExpr").asInstanceOf[js.UndefOr[String]].getOrElse("")
      exprInputElem.oninput = (_: dom.Event) => {
        import scala.util.*
        val newExprText = exprInputElem.value.trim
        val maybeNewExpr = Try(parseExpr(nodeId, newExprText))
        maybeNewExpr match {
          case Failure(e) =>
            if nodeType == Node.Declare then
              programModel.updateDeclare(Request.UpdateDeclare(nodeId, expr = Some(None)))
              node.data("rawExpr", newExprText)
              setLabel()
            else
              EventUtils.dispatchEvent("syntax-error",
                js.Dynamic.literal(msg = e.getMessage)
              )
          case Success(_) =>
            EventUtils.dispatchEvent("syntax-success", null)
            if nodeType == Node.Declare then
              programModel.updateDeclare(Request.UpdateDeclare(nodeId, expr = Some(Some(newExprText))))
            else if nodeType == Node.Assign then
              programModel.updateAssign(Request.UpdateAssign(nodeId, expr = Some(newExprText)))
            else if nodeType == Node.If then
              programModel.updateIf(Request.UpdateIf(nodeId, expr = newExprText))
            else
              programModel.updateOutput(Request.UpdateOutput(nodeId, newExprText))

            node.data("rawExpr", newExprText)
            setLabel()
        }
      }

      val typeSelectElem = select(
        Expression.Type.values.map { tpe =>
          option(value := tpe.toString)(tpe.toString)
        },
        onchange := { (e: dom.Event) =>
          val thisElem = e.target.asInstanceOf[dom.html.Select]
          val newType = Expression.Type.values(thisElem.selectedIndex)
          programModel.updateDeclare(Request.UpdateDeclare(nodeId, tpe = Some(newType)))
          
          node.data("rawTpe", newType.toString)
          setLabel()
        }
      ).render
      val typeLabelElem = label(
        "Type: ",
        typeSelectElem
      ).render

      // clear first, prepare for new inputs
      editWrapperElem.innerText = ""

      // append edit elements
      var hasName = false
      var filledName = false
      if (Set(Node.Declare, Node.Assign, Node.Input).contains(nodeType)) {
        hasName = true
        filledName = nameInputElem.value.nonEmpty
        val editElem = div(nameLabelElem).render
        editWrapperElem.appendChild(editElem)
      }

      var hasExpr = false
      if (Set(Node.Declare, Node.Assign, Node.Output, Node.If).contains(nodeType)) {
        hasExpr = true
        val editElem = div(exprLabelElem).render
        editWrapperElem.appendChild(editElem)
      }

      if (Set(Node.Declare).contains(nodeType)) {
        typeSelectElem.value = varType.get
        val typeElem = div(typeLabelElem).render
        editWrapperElem.appendChild(typeElem)
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



  // (edge, nextNodeId, maybeDummy, direction)
  private def getInsertData(targetNode: js.Any): (js.Dynamic, String, Option[String], String) = {
    val targetNodeDyn = targetNode.asDyn
    if (targetNodeDyn.isEdge().asInstanceOf[Boolean]) {
      val source = targetNodeDyn.source()
      val target = targetNodeDyn.target()
      val (edge, outEdge, nextNode, maybeDummy) = 
        if (source.data("tpe").toString == Node.Dummy) {
          val dummyId = source.data("id").toString
          (source.incomers("edge").first(), targetNodeDyn, target, Some(dummyId))
        } else if (target.data("tpe").toString == Node.Dummy) {
          val dummyId = target.data("id").toString
          (targetNodeDyn, target.outgoers("edge").first(), target.outgoers("node").first(), Some(dummyId))
        } else {
          (targetNodeDyn, targetNodeDyn, target, None)
        }
      (edge, nextNode.data("id").toString, maybeDummy, outEdge.data("dir").toString)
    } else {
      val dummyNodeData = targetNodeDyn.data()
      (targetNodeDyn.incomers("edge").first(), dummyNodeData.endId.toString, Some(targetNodeDyn.data("id").toString), targetNodeDyn.outgoers("edge").first().data("dir").toString)
    }
  }

  // set of nodes to be deleted, until END-ID, e.g. when deleting an IF
  private def getDeleteIds(node: js.Dynamic, maybeEndId: Option[String]): Set[String] = maybeEndId match {
    case None =>
      Set(node.data("id").toString)
    case Some(endId) =>
      val outgoers = node.outgoers("node").toArray().asInstanceOf[js.Array[js.Dynamic]]
      val res = for (outgoer <- outgoers) yield {
        val outgoerId = outgoer.data("id").toString
        if (outgoerId != endId) getDeleteIds(outgoer, Some(endId))
        else Set.empty
      }
      res.flatten.toSet + node.data("id").toString + endId
  }
  
  private def doLayout(): Unit = {
    val layoutOpts = js.Dynamic.literal(
      name = "dagre",
      padding = 10,
      spacingFactor = 0.97,
      nodeSep = 127,
      rankSep = 30,
      rankDir = "TB", // top -> bottom
      animate = true,
      animationDuration = 133
    )
    cy.asDyn.layout(layoutOpts).run()
  }
}