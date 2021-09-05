package dev.sacode.flowrun
package edit

import scalajs.js
import org.scalajs.dom
import dev.sacode.flowrun.cytoscape.*
import dev.sacode.flowrun.ProgramModel.Request

class ContextMenu(programModel: ProgramModel, cy: cytoscape) {
  def setup(): Unit = {
    cy.contextMenus(
      js.Dynamic.literal(
        evtType = "cxttap", // right-click
        menuItems = js.Array(
          js.Dynamic.literal(
            id = "remove",
            content = "remove",
            tooltipText = "Remove statement",
            image =
              js.Dynamic.literal(src = "images/delete.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"node.${Node.Removable}",
            onClickFunction = { (event: dom.Event) =>
              val target = event.target.asDyn

              val prevEdge = target.incomers("edge").first()
              val prevEdgeLabel = prevEdge.data("label").toString
              val prevId = target.incomers("node").first().data("id").toString

              // MAYBE END NODE (IF/WHILE/FOR)
              val maybeEndId = Option(target.data("endId").toString).filterNot(_.trim.isEmpty)
              val nextId = maybeEndId match
                case Some(endId) =>
                  cy.asDyn
                    .nodes(s"node[id = '$endId']")
                    .outgoers("node")
                    .first()
                    .data("id")
                    .toString
                case None =>
                  target.outgoers("node").first().data("id").toString

              val isNextNodeEnd = target.outgoers("node").first().data("tpe").toString == Node.IfEnd

              if (prevEdgeLabel == "true" || prevEdgeLabel == "false") && isNextNodeEnd then
                val dummyNode = Node("", Node.Dummy, startId = prevId, endId = nextId)
                cy.add(dummyNode.toLit)
                cy.add(Edge(dummyNode.id, nextId, dir = "vert").toLit)
                prevEdge.move(js.Dynamic.literal(target = dummyNode.id)) // rebind
              else prevEdge.move(js.Dynamic.literal(target = nextId)) // rebind

              val toDeleteIds = getDeleteIds(target, maybeEndId)
              toDeleteIds.foreach(id => cy.remove(s"node[id = '$id']"))

              doLayout(cy)

              programModel.delete(Request.Delete(target.data("id").toString))
            }
          ),
          js.Dynamic.literal(
            id = "add-output",
            content = "output",
            tooltipText = "Add output statement",
            image =
              js.Dynamic.literal(src = "images/output.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("\"output\"", Node.Output, rawExpr = "\"output\"")
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(
                Edge(
                  newNode.id,
                  nextNodeId,
                  dir = dir,
                  blockId = edge.data("blockId").toString
                ).toLit
              )
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout(cy)

              programModel.addOutput(
                Request.AddOutput(
                  newNode.id,
                  edge.source().data("id").toString,
                  edge.data("blockId").toString
                )
              )
            }
          ),
          js.Dynamic.literal(
            id = "add-input",
            content = "input",
            tooltipText = "Add input statement",
            image =
              js.Dynamic.literal(src = "images/input.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("input", Node.Input, rawName = "input")
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(
                Edge(
                  newNode.id,
                  nextNodeId,
                  dir = dir,
                  blockId = edge.data("blockId").toString
                ).toLit
              )
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout(cy)

              programModel.addInput(
                Request.AddInput(
                  newNode.id,
                  edge.source().data("id").toString,
                  edge.data("blockId").toString
                )
              )
            },
            hasTrailingDivider = true
          ),
          js.Dynamic.literal(
            id = "add-declare",
            content = "declare",
            tooltipText = "Add declare statement",
            image =
              js.Dynamic.literal(src = "images/declare.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("declare", Node.Declare, rawTpe = "Integer")
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(
                Edge(
                  newNode.id,
                  nextNodeId,
                  dir = dir,
                  blockId = edge.data("blockId").toString
                ).toLit
              )
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout(cy)

              programModel.addDeclare(
                Request.AddDeclare(
                  newNode.id,
                  "",
                  Expression.Type.Integer,
                  edge.source().data("id").toString,
                  edge.data("blockId").toString
                )
              )
            }
          ),
          js.Dynamic.literal(
            id = "add-assign",
            content = "assign",
            tooltipText = "Add assign statement",
            image =
              js.Dynamic.literal(src = "images/assign.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("assign", Node.Assign)
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(
                Edge(
                  newNode.id,
                  nextNodeId,
                  dir = dir,
                  blockId = edge.data("blockId").toString
                ).toLit
              )
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout(cy)

              programModel.addAssign(
                Request.AddAssign(
                  newNode.id,
                  edge.source().data("id").toString,
                  edge.data("blockId").toString
                )
              )
            },
            hasTrailingDivider = true
          ),
          js.Dynamic.literal(
            id = "add-call",
            content = "call",
            tooltipText = "Add call statement",
            image =
              js.Dynamic.literal(src = "images/assign.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val newNode = Node("call", Node.Call)
              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)
              cy.add(newNode.toLit)
              edge.move(js.Dynamic.literal(target = newNode.id))
              cy.add(
                Edge(
                  newNode.id,
                  nextNodeId,
                  dir = dir,
                  blockId = edge.data("blockId").toString
                ).toLit
              )
              maybeDummy.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

              doLayout(cy)

              programModel.addCall(
                Request.AddCall(
                  newNode.id,
                  edge.source().data("id").toString,
                  edge.data("blockId").toString
                )
              )
            },
            hasTrailingDivider = true
          ),
          js.Dynamic.literal(
            id = "add-if",
            content = "if",
            tooltipText = "Add if statement",
            image =
              js.Dynamic.literal(src = "images/if.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>

              val target = event.target.asDyn

              val (edge, nextNodeId, maybeDummy, dir) = getInsertData(target)

              val ifEndNode = Node("", Node.IfEnd)
              val ifNode = Node("true", Node.If, endId = ifEndNode.id, rawExpr = "true")
              val trueNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)
              val falseNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)

              cy.add(ifNode.toLit)
              cy.add(ifEndNode.toLit)
              cy.add(trueNode.toLit)
              cy.add(falseNode.toLit)

              edge.move(js.Dynamic.literal(target = ifNode.id))
              val falseEdge = Edge(ifNode.id, falseNode.id, "false")
              val trueEdge = Edge(ifNode.id, trueNode.id, "true")

              cy.add(trueEdge.toLit)
              cy.add(falseEdge.toLit)

              cy.add(Edge(falseNode.id, ifEndNode.id, dir = "vert").toLit)
              cy.add(Edge(trueNode.id, ifEndNode.id, dir = "vert").toLit)
              cy.add(
                Edge(
                  ifEndNode.id,
                  nextNodeId,
                  dir = "vert",
                  blockId = edge.data("blockId").toString
                ).toLit
              )

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

              doLayout(cy)

              programModel.addIf(
                Request.AddIf(
                  ifNode.id,
                  trueEdge.id,
                  falseEdge.id,
                  ifEndNode.id,
                  edge.source().data("id").toString,
                  edge.data("blockId").toString
                )
              )
            }
          )
        )
      )
    )
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
          (
            targetNodeDyn,
            target.outgoers("edge").first(),
            target.outgoers("node").first(),
            Some(dummyId)
          )
        } else {
          (targetNodeDyn, targetNodeDyn, target, None)
        }
      (edge, nextNode.data("id").toString, maybeDummy, outEdge.data("dir").toString)
    } else {
      val dummyNodeData = targetNodeDyn.data()
      (
        targetNodeDyn.incomers("edge").first(),
        dummyNodeData.endId.toString,
        Some(targetNodeDyn.data("id").toString),
        targetNodeDyn.outgoers("edge").first().data("dir").toString
      )
    }
  }

  // set of nodes to be deleted, until END-ID, e.g. when deleting an IF
  private def getDeleteIds(node: js.Dynamic, maybeEndId: Option[String]): Set[String] =
    maybeEndId match {
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
}
