package dev.sacode.flowrun
package edit
package ctxmenu

import scalajs.js
import org.scalajs.dom
import dev.sacode.flowrun.cytoscape.cytoscape
import dev.sacode.flowrun.cytoscape.Node
import dev.sacode.flowrun.edit.ctxmenu.ContextMenuActions

class DesktopContextMenu(programModel: ProgramModel, cy: cytoscape) {

  private val actions = ContextMenuActions(programModel, cy)

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
              actions.removeFunction(target)
            }
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
              actions.addDeclareFunction(target)
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
              actions.addInputFunction(target)
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
              actions.addOutputFunction(target)
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
              actions.addAssignFunction(target)
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
              actions.addCallFunction(target)
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
              actions.addIfFunction(target)
            }
          ),
          js.Dynamic.literal(
            id = "add-while",
            content = "while",
            tooltipText = "Add while statement",
            image =
              js.Dynamic.literal(src = "images/if.svg", width = 12, height = 12, x = 3, y = 4),
            selector = s"edge, node.${Node.Dummy}",
            onClickFunction = { (event: dom.Event) =>
              val target = event.target.asDyn
              actions.addWhileFunction(target)
            }
          )
        )
      )
    )
  }

}
