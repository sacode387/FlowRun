package ba.sake.flowrun
package edit

import scalajs.js
import org.scalajs.dom
import reactify.*
import ba.sake.flowrun.cytoscape.*
import ba.sake.flowrun.parse.*

class FunctionEditor(
  programModel: ProgramModel,
  flowrunChannel: Channel[FlowRun.Event],
  flowRunElements: FlowRunElements
) {
  
  private val cy = new cytoscape(
    js.Dynamic.literal(
      container = flowRunElements.drawArea,
      userZoomingEnabled = false,
      userPanningEnabled = false,
      boxSelectionEnabled = false,
      maxZoom = 1.3,
      autoungrabify = true,
      style = utils.styleJson
    )
  )

  loadCurrentFunction()
  ContextMenu(programModel, cy).setup()
  EditPanel(programModel, flowRunElements, flowrunChannel, cy).setup()
  

  cy.asDyn.on("unselect add remove", "node", (evt: js.Dynamic) => {
    // node not selected anymore, hide the inputs...
    flowRunElements.editStatement.innerText = ""
    cy.asDyn.nodes().unselect()
    // clear visual error on nodes
    clearErrors()
  })

  flowrunChannel.attach {
    case FlowRun.Event.EvalError(nodeId, msg) =>
      cy.asDyn.nodes(s"node[id = '$nodeId']").data("has-error", true)
    case _ =>
  }

  def clearErrors(): Unit =
    cy.asDyn.nodes().data("has-error", false)
    flowrunChannel := FlowRun.Event.SyntaxSuccess

  def loadCurrentFunction(): Unit = {
    //println("BEFORE: " + js.JSON.stringify(cy.asDyn.elements().jsons()))
    cy.remove("*")
    val statements = programModel.currentFunction.statements

    val firstNode = Node("begin", Node.Begin, id = "beginId")
    val lastNode = Node("end", Node.End, id = "endId")
    cy.add(firstNode.toLit)
    cy.add(lastNode.toLit)
    val firstEdge = cy.add(Edge(firstNode.id, lastNode.id).toLit)

    load(statements, firstNode, firstEdge)
    doLayout(cy)
  }
  
  private def load(statements: List[Statement], lastNode: Node, lastEdge: js.Dynamic): js.Dynamic = {
    import Statement.*

    var prevNode = lastNode
    var prevEdge = lastEdge

    statements.foreach {
      case stmt: Start =>
        val rawParams = stmt.parameters.map(_._1).mkString(",")
        val newNode = Node(stmt.label, Node.Start, id = stmt.id, rawName = stmt.name, rawParams = rawParams)
        cy.add(newNode.toLit)
        prevEdge = cy.add(Edge(newNode.id, newNode.id).toLit)
        prevNode = newNode
        
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
        val newNode = Node(stmt.label, Node.Declare, id = stmt.id, rawName = stmt.name, rawTpe = stmt.tpe.toString, rawExpr = stmt.initValue.getOrElse(""))
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
      case stmt: Call =>
        val newNode = Node(stmt.label, Node.Call, id = stmt.id, rawExpr = stmt.value)
        cy.add(newNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
        prevNode = newNode
        prevEdge = cy.add(Edge(newNode.id, newNode.id).toLit)
      case Block(_, blockStats) =>
        // noop
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
          load(trueBlock.statements, ifNode, trueEdge)
        
        val lastFalseEdge = if falseBlock.statements.isEmpty then
          val falseNode = Node("", Node.Dummy, startId = ifNode.id, endId = ifEndNode.id)
          cy.add(falseNode.toLit)
          falseEdge.move(js.Dynamic.literal(target = falseNode.id))
          cy.add(Edge(falseNode.id, falseNode.id).toLit)
        else
          load(falseBlock.statements, ifNode, falseEdge)

        lastTrueEdge.move(js.Dynamic.literal(target = ifEndNode.id))
        lastFalseEdge.move(js.Dynamic.literal(target = ifEndNode.id))

        prevEdge = cy.add(Edge(ifEndNode.id, ifEndNode.id, dir = "vert", blockId = trueBlock.id).toLit)
        prevNode = ifEndNode

      case stmt: Return =>
        val newNode = Node(stmt.label, Node.Return, id = stmt.id, rawExpr = stmt.maybeValue.getOrElse(""))
        cy.add(newNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
        prevNode = newNode

      case stmt @ (_: Dummy | _: BlockEnd) =>
        val nodeType = stmt.getClass.getSimpleName.reverse.dropWhile(_ == '$').reverse
        println("tpe: " + nodeType)
        val newNode = Node(stmt.label, nodeType, id = stmt.id)
        cy.add(newNode.toLit)
        prevEdge.move(js.Dynamic.literal(target = newNode.id))
    }
    prevEdge
  }

}