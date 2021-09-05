package dev.sacode.flowrun
package edit

import scalajs.js
import org.scalajs.dom
import dev.sacode.flowrun.cytoscape.*
import dev.sacode.flowrun.ProgramModel.Request
import dev.sacode.flowrun.edit.ctxmenu.ContextMenuActions

class TouchContextMenu(programModel: ProgramModel, cy: cytoscape) {

  private val actions = ContextMenuActions(programModel, cy)

  def setup(): Unit = {
    cy.cxtmenu(
      js.Dynamic.literal(
        selector = s"edge",
        commands = js.Array(
          js.Dynamic.literal(
            id = "add-output",
            content = "output",
            tooltipText = "Add output statement",
            image =
              js.Dynamic.literal(src = "images/output.svg", width = 12, height = 12, x = 3, y = 4),
            select = { (elem: js.Dynamic) =>
              actions.addOutputFunction(elem)
            }
          )
        )
      )
    )
  }
}
