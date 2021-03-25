package ba.sake.flowrun
package cytoscape

import scalajs.js
import js.annotation._
import org.scalajs.dom

import ba.sake.flowrun.parse._, Expression.Type

class CytoscapeFlowchart(
  container: dom.Element,
  editWrapperElem: dom.Element,
  programModel: ProgramModel
) {
  
  val cy = new cytoscape(
    js.Dynamic.literal(
      container = container,
      userZoomingEnabled = false,
      userPanningEnabled = false,
      boxSelectionEnabled = false,
      maxZoom = 1.3,
      autoungrabify = true,
      style = utils.styleJson,
      elements = js.Dynamic.literal(
        nodes = js.Array(
          Node("begin", Node.BeginEnd, 70, 30, id = "beginId").toLit,
          Node("end", Node.BeginEnd, 70, 30, id = "endId").toLit
        ),
        edges = js.Array(
          Edge("beginId", "endId").toLit
        )
      )
    )
  )

  setupMenus()
  setupEditPanel()
  doLayout()

  def clearErrors(): Unit =
    cy.asDyn.nodes().data("has-error", false)

  dom.document.addEventListener("eval-error", (e: dom.CustomEvent) => {
    val nodeId = e.detail.asDyn.nodeId
    cy.asDyn.nodes(s"node[id = '$nodeId']").data("has-error", true)
  })

  private def setupMenus(): Unit = {
    cy.contextMenus(
      js.Dynamic.literal(
        evtType = "cxttap", // cxttap == right-click, tap == click
        menuItems = js.Array(
          js.Dynamic.literal(
            id = "remove",
            content = "remove",
            tooltipText = "Remove statement",
            image = js.Dynamic.literal(src = "images/delete.svg", width = 12, height = 12, x = 3, y = 4),
            selector = "node.if, node.input, node.output, node.declare, node.assign",
            onClickFunction = { (event: dom.Event) =>
              val target = event.target.asDyn

              if (target.data("tpe").toString == Node.If) {
                val endId = target.data("endId").toString

                val prevEdge = target.incomers("edge").first()
                val nextId = cy.asDyn.nodes(s"node[id = '$endId']").outgoers("node").first().data("id").toString

                prevEdge.move(js.Dynamic.literal(target = nextId))
                val toDeleteIds = getDeleteIds(target, endId)
                toDeleteIds.foreach(id => cy.remove(s"node[id = '$id']"))
              } else {
                val ins = target.incomers("node").toArray().asInstanceOf[js.Array[js.Object]].toSeq
                val outs = target.outgoers("node").toArray().asInstanceOf[js.Array[js.Object]].toSeq
                target.remove()
                // rebind remaining nodes
                ins.foreach { inNode =>
                  val inNodeId = inNode.asDyn.id().toString
                  outs.foreach { outNode =>
                    val outNodeId = outNode.asDyn.id().toString
                    cy.add(Edge(inNodeId, outNodeId).toLit)
                  }
                }
              }

              // TODO skontat ako je true/false
              // DODAT DUMMY OPET !!! ?
              
              doLayout()

              programModel.delete(target.data("id").toString)
            }
          ),
          js.Dynamic.literal(
            id = "add-output",
            content = "output",
            tooltipText = "Add output statement",
            image = js.Dynamic.literal(src = "images/output.svg", width = 12, height = 12, x = 3, y = 4),
            selector = "edge, node.dummy",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("output", Node.Output)
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))
              
              doLayout()

              programModel.addOutput(newNode.id)(edge.source().data("id").toString, edge.data("blockId").toString)
            }
          ),
          js.Dynamic.literal(
            id = "add-input",
            content = "input",
            tooltipText = "Add input statement",
            image = js.Dynamic.literal(src = "images/input.svg", width = 12, height = 12, x = 3, y = 4),
            selector = "edge, node.dummy",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("input", Node.Input)
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))
              
              doLayout()
            },
            hasTrailingDivider = true
          ),
          js.Dynamic.literal(
            id = "add-declare",
            content = "declare",
            tooltipText = "Add declare statement",
            image = js.Dynamic.literal(src = "images/declare.svg", width = 12, height = 12, x = 3, y = 4),
            selector = "edge, node.dummy",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("declare", Node.Declare, rawTpe = "Int")
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout()

              programModel.addDeclare(newNode.id, "", Type.Integer)(edge.source().data("id").toString, edge.data("blockId").toString)
            }
          ),
          js.Dynamic.literal(
            id = "add-assign",
            content = "assign",
            tooltipText = "Add assign statement",
            image = js.Dynamic.literal(src = "images/assign.svg", width = 12, height = 12, x = 3, y = 4),
            selector = "edge, node.dummy",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("assign", Node.Assign)
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(Edge(newNode.id, nextNodeId, dir = dir, blockId = edge.data("blockId").toString).toLit)
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout()

              programModel.addAssign(newNode.id)(edge.source().data("id").toString, edge.data("blockId").toString)
            },
            hasTrailingDivider = true
          ),
          js.Dynamic.literal(
            id = "add-if",
            content = "if",
            tooltipText = "Add if statement",
            image = js.Dynamic.literal(src = "images/if.svg", width = 12, height = 12, x = 3, y = 4),
            selector = "edge, node.dummy",
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

              programModel.addIf(ifNode.id, trueEdge.id, falseEdge.id, ifEndNode.id)(edge.source().data("id").toString, edge.data("blockId").toString)
            }
          )
        )
      )
    )
  }

  private def setupEditPanel(): Unit = {   
    cy.asDyn.on("select", "node.editable", (evt: js.Dynamic) => {
      val node = evt.target
      val nodeId = node.data("id").toString
      val nodeType = node.data("tpe").toString
      
      val nameLabelElem = dom.document.createElement("label").asInstanceOf[dom.html.Label]
      val nameInputElem = dom.document.createElement("input").asInstanceOf[dom.html.Input]
      nameLabelElem.appendChild(dom.document.createTextNode("Name: "))
      nameLabelElem.appendChild(nameInputElem)
      nameInputElem.value = node.data("rawName").asInstanceOf[js.UndefOr[String]].getOrElse("")
      
      val exprLabelElem = dom.document.createElement("label").asInstanceOf[dom.html.Label]
      val exprInputElem = dom.document.createElement("input").asInstanceOf[dom.html.Input]
      exprLabelElem.appendChild(dom.document.createTextNode("Expression: "))
      exprLabelElem.appendChild(exprInputElem)
      exprInputElem.value = node.data("rawExpr").asInstanceOf[js.UndefOr[String]].getOrElse("")

      nameInputElem.oninput = (event: dom.Event) => {
        val newName = nameInputElem.value
        if nodeType == Node.Declare then
          programModel.updateDeclare(nodeId, name = Some(newName))
        else
          programModel.updateAssign(nodeId, name = Some(newName))
        
        node.data("rawName", newName)
        val newLabel = getLabel()
        node.data("label", newLabel)
        node.data("width", 55 max newLabel.length * 11)
      }

      exprInputElem.oninput = (event: dom.Event) => {
        val newExprText = exprInputElem.value.trim
        val newExpr = if newExprText.isEmpty then None
          else Some(parseExpr(nodeId, exprInputElem.value))
        if nodeType == Node.Declare then
          programModel.updateDeclare(nodeId, expr = newExpr)
        else if nodeType == Node.Assign then
          programModel.updateAssign(nodeId, expr = newExpr)
        else if nodeType == Node.If then
          programModel.updateIf(nodeId, expr = newExpr.getOrElse(parseExpr(nodeId, "true")))
        else
          programModel.updateOutput(nodeId, newExpr.getOrElse(parseExpr(nodeId, "\"\"")))
        
        node.data("rawExpr", exprInputElem.value)
        val newLabel = getLabel()
        node.data("label", newLabel)
        node.data("width", 55 max newLabel.length * 11)
      }

      def getLabel(): String =
        val maybeName = node.data("rawName").asInstanceOf[js.UndefOr[String]].toOption
        val maybeExpr = node.data("rawExpr").asInstanceOf[js.UndefOr[String]].toOption.filterNot(_.trim.isEmpty)
        val maybeExprText = maybeExpr.map(e => s" = $e").getOrElse("")
        if nodeType == Node.Declare then
          s"""${maybeName.get}: ${node.data("rawTpe")}$maybeExprText"""
        else if nodeType == Node.Assign then
          s"""${maybeName.get}$maybeExprText"""
        else maybeExpr.getOrElse("")

      editWrapperElem.innerText = "" // clear

      // append edit elements
      if (Set(Node.Declare, Node.Assign).contains(nodeType)) {
        val editElem = dom.document.createElement("div")
        editElem.appendChild(nameLabelElem)
        editWrapperElem.appendChild(editElem)
      }
      locally {
        val editElem = dom.document.createElement("div")
        editElem.appendChild(exprLabelElem)
        editWrapperElem.appendChild(editElem)
      }
      
      
    })

    cy.asDyn.on("unselect add remove", "node", (evt: js.Dynamic) => {
      editWrapperElem.innerText = "" // clear
      cy.asDyn.nodes().unselect()
      clearErrors()
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

  // set of nodes to be deleted, e.g. when deleting an IF
  private def getDeleteIds(node: js.Dynamic, endId: String): Set[String] = {
    val outgoers = node.outgoers("node").toArray().asInstanceOf[js.Array[js.Dynamic]]
    val res = for (outgoer <- outgoers) yield {
      val outgoerId = outgoer.data("id").toString
      if (outgoerId != endId) getDeleteIds(outgoer, endId)
      else Set.empty
    }
    res.flatten.toSet + node.data("id").toString + endId
  }
  
  private def doLayout(): Unit = {
    val layoutOpts = js.Dynamic.literal(
      name = "dagre",
      padding = 10,
      spacingFactor = 0.97,
      nodeSep = 70,
      rankSep = 30,
      rankDir = "TB", // top -> bottom
      animate = true,
      animationDuration = 133
    )
    cy.asDyn.layout(layoutOpts).run()
  }

}