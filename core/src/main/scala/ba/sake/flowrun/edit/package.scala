package ba.sake.flowrun
package edit

import scalajs.js
import ba.sake.flowrun.cytoscape.cytoscape

def doLayout(cy: cytoscape): Unit = {
  val layoutOpts = js.Dynamic.literal(
    name = "dagre",
    padding = 10,
    spacingFactor = 0.97,
    nodeSep = 127,
    rankSep = 30,
    rankDir = "TB", // top -> bottom
    animate = true,
    animationDuration = 155
  )
  cy.asDyn.layout(layoutOpts).run()
}