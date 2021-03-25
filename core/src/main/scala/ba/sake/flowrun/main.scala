package ba.sake.flowrun

import org.scalajs.dom
import org.scalajs.dom.window
import org.scalajs.dom.window.document
import ba.sake.flowrun.cytoscape.CytoscapeFlowchart
import ba.sake.flowrun.eval.Interpreter
import ba.sake.flowrun.parse._

@main def start(): Unit = {
  
  window.onload = (e: dom.Event) => {
    
    val container = document.getElementById("cy")
    val outputElem = document.getElementById("run-output")
    val runBtn = document.getElementById("run-btn").asInstanceOf[dom.html.Button]
    val editWrapperElem = dom.window.document.getElementById("edit-wrapper")

    val programModel = ProgramModel(Program())
    val cytoscapeFlowchart = CytoscapeFlowchart(container, editWrapperElem, programModel)

    runBtn.onclick = _ => {
      cytoscapeFlowchart.clearErrors()
      Interpreter(programModel, outputElem).run()
    }
  }

  
  //val expr = """x==5>1<11+(2-3<=7>=*4/5)6== "a   b"z!abc!=def||nesto&&truea-false*"""
  //val expr = """1+-*/%() > < >= <= == ! != true false null nil "bla" "" && || 2.a """
  //val lexer = Lexer("123node", expr)
  //println(expr)
  //println(lexer.lex())

  
  val inputttttt = "(7)"
  val ast = parseExpr("test", inputttttt)
  //pprint.pprintln(ast)

  //val res = Interpreter(programModel, outputElem)
}
