package dev.sacode.flowrun
package edit
package ctxmenu

import scalajs.js
import org.scalajs.dom
import dev.sacode.flowrun.cytoscape.*
import dev.sacode.flowrun.ProgramModel.Request

class ContextMenuActions(
    programModel: ProgramModel,
    cy: cytoscape
) {

  def removeFunction(target: js.Dynamic) = {

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
      cy.add(Edge(dummyNode.id, nextId, dir = "down").toLit)
      prevEdge.move(js.Dynamic.literal(target = dummyNode.id)) // rebind
    else prevEdge.move(js.Dynamic.literal(target = nextId)) // rebind

    val toDeleteIds = getDeleteIds(target, maybeEndId)
    toDeleteIds.foreach(id => cy.remove(s"node[id = '$id']"))

    doLayout(cy)

    programModel.delete(Request.Delete(target.data("id").toString))
  }

  def addOutputFunction(target: js.Dynamic) = {

    val newNode = Node("\"output\"", Node.Output, rawExpr = "\"output\"")
    val (inEdge, outEdge, maybeDummyId) = getInsertData(target)
    cy.add(newNode.toLit)
    inEdge.move(js.Dynamic.literal(target = newNode.id))
    outEdge.move(js.Dynamic.literal(source = newNode.id))
    maybeDummyId.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

    doLayout(cy)

    programModel.addOutput(
      Request.AddOutput(
        newNode.id,
        inEdge.source().data("id").toString,
        inEdge.data("blockId").toString
      )
    )
  }

  def addInputFunction(target: js.Dynamic) = {

    val newNode = Node("input", Node.Input, rawName = "input")
    val (inEdge, outEdge, maybeDummyId) = getInsertData(target)
    cy.add(newNode.toLit)
    inEdge.move(js.Dynamic.literal(target = newNode.id))
    outEdge.move(js.Dynamic.literal(source = newNode.id))
    maybeDummyId.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

    doLayout(cy)

    programModel.addInput(
      Request.AddInput(
        newNode.id,
        inEdge.source().data("id").toString,
        inEdge.data("blockId").toString
      )
    )
  }

  def addDeclareFunction(target: js.Dynamic) = {

    val newNode = Node("declare", Node.Declare, rawTpe = "Integer")
    val (inEdge, outEdge, maybeDummyId) = getInsertData(target)
    cy.add(newNode.toLit)
    inEdge.move(js.Dynamic.literal(target = newNode.id))
    outEdge.move(js.Dynamic.literal(source = newNode.id))
    maybeDummyId.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

    doLayout(cy)

    programModel.addDeclare(
      Request.AddDeclare(
        newNode.id,
        "",
        Expression.Type.Integer,
        inEdge.source().data("id").toString,
        inEdge.data("blockId").toString
      )
    )
  }

  def addAssignFunction(target: js.Dynamic) = {

    val newNode = Node("assign", Node.Assign)
    val (inEdge, outEdge, maybeDummyId) = getInsertData(target)
    cy.add(newNode.toLit)
    inEdge.move(js.Dynamic.literal(target = newNode.id))
    outEdge.move(js.Dynamic.literal(source = newNode.id))
    maybeDummyId.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

    doLayout(cy)

    programModel.addAssign(
      Request.AddAssign(
        newNode.id,
        inEdge.source().data("id").toString,
        inEdge.data("blockId").toString
      )
    )
  }

  def addCallFunction(target: js.Dynamic) = {

    val newNode = Node("call", Node.Call)
    val (inEdge, outEdge, maybeDummyId) = getInsertData(target)
    cy.add(newNode.toLit)
    inEdge.move(js.Dynamic.literal(target = newNode.id))
    outEdge.move(js.Dynamic.literal(source = newNode.id))
    maybeDummyId.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

    doLayout(cy)

    programModel.addCall(
      Request.AddCall(
        newNode.id,
        inEdge.source().data("id").toString,
        inEdge.data("blockId").toString
      )
    )
  }

  def addIfFunction(target: js.Dynamic) = {

    val (inEdge, outEdge, maybeDummyId) = getInsertData(target)

    val ifEndNode = Node("", Node.IfEnd)
    val ifNode = Node("true", Node.If, endId = ifEndNode.id, rawExpr = "true")
    val trueNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)
    val falseNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)

    cy.add(ifNode.toLit)
    cy.add(ifEndNode.toLit)
    cy.add(trueNode.toLit)
    cy.add(falseNode.toLit)

    inEdge.move(js.Dynamic.literal(target = ifNode.id))
    val falseEdge = Edge(ifNode.id, falseNode.id, "false", dir = "left")
    val trueEdge = Edge(ifNode.id, trueNode.id, "true", dir = "right")

    cy.add(trueEdge.toLit)
    cy.add(falseEdge.toLit)

    cy.add(Edge(falseNode.id, ifEndNode.id, dir = "down").toLit)
    cy.add(Edge(trueNode.id, ifEndNode.id, dir = "down").toLit)
    outEdge.move(js.Dynamic.literal(source = ifEndNode.id))

    maybeDummyId.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

    doLayout(cy)

    programModel.addIf(
      Request.AddIf(
        ifNode.id,
        trueEdge.id,
        falseEdge.id,
        ifEndNode.id,
        inEdge.source().data("id").toString,
        inEdge.data("blockId").toString
      )
    )
  }

  def addWhileFunction(target: js.Dynamic) = {

    val (inEdge, outEdge, maybeDummyId) = getInsertData(target)

    val whileEndNode = Node("", Node.WhileEnd)
    val whileNode = Node("true", Node.While, endId = whileEndNode.id, rawExpr = "true")
    val trueNode = Node("", Node.Dummy, startId = whileNode.id, endId = whileNode.id)

    cy.add(whileNode.toLit)
    cy.add(whileEndNode.toLit)
    cy.add(trueNode.toLit)

    inEdge.move(js.Dynamic.literal(target = whileNode.id))
    val falseEdge = Edge(whileNode.id, whileEndNode.id, "false", dir = "left")
    val trueEdge = Edge(whileNode.id, trueNode.id, "true", dir = "right")

    cy.add(trueEdge.toLit)
    cy.add(falseEdge.toLit)

    cy.add(Edge(trueNode.id, whileNode.id, dir = "left").toLit)
    outEdge.move(js.Dynamic.literal(source = whileEndNode.id))

    maybeDummyId.foreach(dummyId => cy.remove(s"node[id = '$dummyId']"))

    doLayout(cy)

    programModel.addWhile(
      Request.AddWhile(
        whileNode.id,
        trueEdge.id,
        falseEdge.id,
        whileEndNode.id,
        inEdge.source().data("id").toString,
        inEdge.data("blockId").toString
      )
    )
  }

  // (inEdge, outEdge, maybeDummyId)
  private def getInsertData(targetNode: js.Any): (js.Dynamic, js.Dynamic, Option[String]) = {
    val targetNodeDyn = targetNode.asDyn
    if (targetNodeDyn.isEdge().asInstanceOf[Boolean]) {
      val targetEdgeDyn = targetNodeDyn
      val source = targetEdgeDyn.source()
      val target = targetEdgeDyn.target()
      if (source.data("tpe").toString == Node.Dummy) {
        val dummyId = source.data("id").toString
        (source.incomers("edge").first(), source.outgoers("edge").first(), Some(dummyId))
      } else if (target.data("tpe").toString == Node.Dummy) {
        val dummyId = target.data("id").toString
        (
          target.incomers("edge").first(),
          target.outgoers("edge").first(),
          Some(dummyId)
        )
      } else {
        val edgeCopy = Edge(source.data("id").toString, target.data("id").toString, dir = "down")
        val outEdge = cy.add(edgeCopy.toLit)
        (targetEdgeDyn, outEdge, None)
      }
    } else {
      (
        targetNodeDyn.incomers("edge").first(),
        targetNodeDyn.outgoers("edge").first(),
        Some(targetNodeDyn.data("id").toString)
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
