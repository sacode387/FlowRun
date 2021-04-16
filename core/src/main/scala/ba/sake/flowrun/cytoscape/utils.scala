package ba.sake.flowrun.cytoscape

import scalajs.js
import org.scalajs.dom

object utils {

  val styleJson = js.JSON.parse(s"""[
    {
      "selector": "node",
      "style": {
        "overlay-opacity": 0,
        "font-family": "Ubuntu Mono, monospace",
        "font-size": "14px"
      }
    }, {
      "selector": "node",
      "style": {
        "label": "data(label)",
        "width": "data(width)",
        "height": "data(height)",
        "text-valign": "center",
        "text-halign": "center"
      }
    }, {
      "selector": "node.${Node.Editable}:selected[!has-error]",
      "style": {
        "border-width": "1px",
        "border-style": "dashed",
        "border-opacity": 0.5
      }
    }, {
      "selector": "node[?has-error]",
      "style": {
        "border-width": "4px",
        "border-style": "dashed",
        "border-color": "red"
      }
    }, {
      "selector": "node.${Node.Begin}, node.${Node.End}",
      "style": {
        "shape": "ellipse",
        "background-color": "#00bfff",
        "color": "white"
      }
    }, {
      "selector": "node.${Node.Declare}",
      "style": {
        "shape": "rectangle",
        "background-color": "#191970",
        "color": "white"
      }
    }, {
      "selector": "node.${Node.Assign}",
      "style": {
        "shape": "rectangle",
        "background-color": "#556b2f",
        "color": "white"
      }
    }, {
      "selector": "node.${Node.Call}",
      "style": {
        "shape": "rectangle",
        "background-color": "#ff8303",
        "color": "white"
      }
    }, {
      "selector": "node.${Node.If}",
      "style": {
        "shape": "diamond",
        "background-color": "#00ff00",
        "color": "black"
      }
    }, {
      "selector": "node.${Node.IfEnd}",
      "style": {
        "shape": "ellipse",
        "background-color": "#000"
      }
    }, {
      "selector": "node.${Node.Output}",
      "style": {
        "label": "data(rawExpr)",
        "background-color": "#0000ff",
        "color": "white",
        "shape": "polygon",
        "shape-polygon-points": [-0.63, -1, 0.63, -1, 1, 1, -1, 1, -0.63, -1]
      }
    }, {
      "selector": "node.${Node.Input}",
      "style": {
        "label": "data(rawName)",
        "background-color": "#ffc266",
        "color": "black",
        "shape": "polygon",
        "shape-polygon-points": [-1, -1, 1, -1, 0.63, 1, -0.63, 1, -1, -1]
      }
    }, {
      "selector": "node.Loop",
      "style": {
        "shape": "diamond",
        "background-color": "#ff4500",
        "color": "white"
      }
    }, {
      "selector": "node.${Node.Dummy}",
      "style": {
        "shape": "star",
        "background-color": "#fff",
        "border-width": "1px"
      }
    }, {
      "selector": "edge",
      "style": {
        "overlay-opacity": 0,
        "font-family": "Ubuntu Mono, monospace",
        "font-size": "12px",
        "label": "data(label)",
        "text-margin-y": -10,
        "width": 2,
        "line-color": "black",
        "target-arrow-color": "black",
        "target-arrow-shape": "triangle",
        "curve-style": "taxi",
        "taxi-direction": "horizontal",
        "taxi-turn": "90%"
      }
    }, {
      "selector": "edge[dir = 'vert']",
      "style": {
        "taxi-direction": "vertical"
      }
    }, {
      "selector": "edge[label = 'false']",
      "style": {
        "line-color": "red",
        "target-arrow-color": "red"
      }
    }, {
      "selector": "edge[label = 'true']",
      "style": {
        "line-color": "green",
        "target-arrow-color": "green"
      }
    }
  ]""")

}
