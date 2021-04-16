package ba.sake.flowrun

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.document

@JSExportTopLevel("start")
@main def start(): Unit =
  dom.window.onload = _ => { 
    val container = document.querySelector("#program-wrapper #cy")
    val editWrapperElem = document.querySelector("#edit-wrapper")
    val runBtn = document.querySelector("#actions-wrapper #run-btn").asInstanceOf[dom.html.Button]
    val functionsElem = document.querySelector("#actions-wrapper #function-chooser")
    val outputElem = document.querySelector("#run-output")
    val variablesElem = document.querySelector("#variables-output")

    val flowRunElements = FlowRunElements(container, editWrapperElem,runBtn,functionsElem,outputElem,variablesElem)
    val program = locally {
      val main = Function("main", None, List(
        Statement.Begin, 
        Statement.Declare("1", "x", Expression.Type.Integer, Option("1")),
        Statement.Call("2", "fun1()"),
        Statement.Output("3", """ "X in main: "+x """),
    //   Statement.Output("4", "x"),
        
      /*  Statement.If("100", "true",
          Statement.Block("101", List(Statement.Output("101-1", "9"))),
          Statement.Block("102", List(
            Statement.If("200", "true",
              Statement.Block("201", List(Statement.Output("201-1", "9"))),
              Statement.Block("202", List(Statement.Output("202-1", "9"))),
            ),
          )),
        ),*/

        Statement.End
      ))

      val fun1 = Function("fun1", None, List(
        Statement.Begin, 
        Statement.Declare("1", "x", Expression.Type.Integer, Option("2")),
        Statement.Output("4", "\"fun1 hello!\""),
        Statement.Output("5", """ "X in fun1: "+x """),
        Statement.End
      ))

      Program("program", main, List(fun1))
    }

    val flowRun = FlowRun(program, flowRunElements)
    println(js.JSON.stringify(flowRun.json()))
  }

