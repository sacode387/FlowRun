package dev.sacode.flowrun
package edit
package ctxmenu

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
        selector = s"node.${Node.Removable}",
        menuRadius = 50,
        commands = js.Array(
          js.Dynamic.literal(
            id = "remove",
            content = """REMOVE <span class="gg-remove-r" style="margin: auto"></span>""",
            fillColor = "rgba(255, 0, 0, 0.85)",
            select = { (elem: js.Dynamic) =>
              actions.removeFunction(elem)
            }
          )
        )
      )
    )

    cy.cxtmenu(
      js.Dynamic.literal(
        selector = s"edge, node.${Node.Dummy}",
        menuRadius = 100,
        commands = js.Array(
          js.Dynamic.literal(
            id = "add-output",
            content = """OUTPUT <span class="gg-screen" style="margin: auto"></span>""",
            fillColor = "#0000FFD9",
            contentStyle = js.Dynamic.literal(color = "#FFF"),
            select = { (elem: js.Dynamic) =>
              actions.addOutputFunction(elem)
            }
          ),
          js.Dynamic.literal(
            id = "add-input",
            content = """INPUT <span class="gg-keyboard" style="margin: auto"></span>""",
            fillColor = "#ffc266D9",
            contentStyle = js.Dynamic.literal(color = "#000"),
            select = { (elem: js.Dynamic) =>
              actions.addInputFunction(elem)
            }
          ),
          js.Dynamic.literal(
            id = "add-declare",
            content = """DECLARE <span class="gg-file-document" style="margin: auto"></span>""",
            fillColor = "#191970D9",
            contentStyle = js.Dynamic.literal(color = "#FFF"),
            select = { (elem: js.Dynamic) =>
              actions.addDeclareFunction(elem)
            }
          ),
          js.Dynamic.literal(
            id = "add-assign",
            content = """ASSIGN <span class="gg-arrows-exchange-v" style="margin: auto"></span>""",
            fillColor = "#556b2fD9",
            contentStyle = js.Dynamic.literal(color = "#FFF"),
            select = { (elem: js.Dynamic) =>
              actions.addAssignFunction(elem)
            }
          ),
          js.Dynamic.literal(
            id = "add-call",
            content = """CALL <span class="gg-bolt" style="margin: auto"></span>""",
            fillColor = "#ff8303D9",
            contentStyle = js.Dynamic.literal(color = "#FFF"),
            select = { (elem: js.Dynamic) =>
              actions.addCallFunction(elem)
            }
          ),
          js.Dynamic.literal(
            id = "add-if",
            content = """IF <span class="gg-shape-rhombus" style="margin: auto"></span>""",
            fillColor = "#00ff00D9",
            contentStyle = js.Dynamic.literal(color = "#000"),
            select = { (elem: js.Dynamic) =>
              actions.addIfFunction(elem)
            }
          )
        )
      )
    )

  }
}
