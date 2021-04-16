package ba.sake.flowrun

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.window
import org.scalajs.dom.window.document
import org.getshaka.nativeconverter.NativeConverter
import scalatags.JsDom.all._

import ba.sake.flowrun.cytoscape.CytoscapeFlowchart
import ba.sake.flowrun.eval._
import ba.sake.flowrun.parse.parseExpr

@JSExportTopLevel("start")
@main def start(): Unit =
  window.onload = _ => init()

def init(): Unit = {
  val container = document.querySelector("#program-wrapper #cy")
  
  val editWrapperElem = document.querySelector("#edit-wrapper")

  val runBtn = document.querySelector("#actions-wrapper #run-btn").asInstanceOf[dom.html.Button]
  val functionsElem = document.querySelector("#actions-wrapper #function-chooser")

  val outputElem = document.querySelector("#run-output")

  val variablesElem = document.querySelector("#variables-output")

  val main = Function("main", None, List(
    Statement.Begin, 
    Statement.Declare("1", "x", Expression.Type.Integer, Option("1")),
    Statement.Call("2", "fun1()"),
    Statement.Output("3", "x"),
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
    Statement.Output("5", "x"),
    Statement.End
  ))

  val programModel = ProgramModel(Program("program", main, List(fun1)))
  val cytoscapeFlowchart = CytoscapeFlowchart(programModel, container, editWrapperElem)

  var interpreter = Interpreter(programModel)

  var lastRun: String = ""

  populateFunctions()

  def populateFunctions(): Unit =
    val allFunctions = List(programModel.ast.main) ++ programModel.ast.functions
    val selectElem = div(
      allFunctions.map { f =>
        val maybeSelected = Option.when(f.name == programModel.currentFunctionName)(checked)
        val maybeDelete = Option.when(f.name != "main") {
          button("Delete", onclick := { (e: dom.Event) =>
            programModel.deleteFunction(f.name)
            programModel.currentFunctionName = "main"
            cytoscapeFlowchart.loadCurrentFunction()
            populateFunctions()
          })
        }
        div(
          label(
            input(
              tpe := "radio", name := "currentFunction", value := f.name, maybeSelected,
              onchange := { (e: dom.Event) =>
                val selectedFunName = e.target.asInstanceOf[dom.html.Input].value
                programModel.currentFunctionName = selectedFunName
                cytoscapeFlowchart.loadCurrentFunction()
              }
            ),
            f.name,
            maybeDelete
          )
        )
      },
      button("Add", onclick := { (e: dom.Event) =>
        val newFunName = "fun123"
        val newFun = Function(newFunName)
        programModel.addFunction(newFun)
        programModel.currentFunctionName = newFunName
        cytoscapeFlowchart.loadCurrentFunction()
        populateFunctions()
      })
    )
    functionsElem.innerText = ""
    functionsElem.appendChild(selectElem.render)

  // run the program
  runBtn.onclick = _ => {
    cytoscapeFlowchart.clearErrors()
    lastRun = getNowTime
    outputElem.innerText = s"Started at: $lastRun"

    interpreter = Interpreter(programModel) // fresh SymTable etc
    interpreter.run()
  }

  // append error
  dom.document.addEventListener("eval-error", (e: dom.CustomEvent) => {
    var msg = s"Started at: $lastRun"
    msg += "\nError: " + e.detail.asDyn.msg
    displayError(msg)
  })
  dom.document.addEventListener("syntax-error", (e: dom.CustomEvent) => {
    var msg = s"Started at: $lastRun"
    msg += "\nError: " + e.detail.asDyn.msg
    displayError(msg)
  })
  dom.document.addEventListener("syntax-success", (e: dom.CustomEvent) => {
    outputElem.innerText = ""
    outputElem.classList.remove("error")
  })
  

  // append new output
  dom.document.addEventListener("eval-output", (e: dom.CustomEvent) => {
    val newOutput = pre(e.detail.asDyn.output.toString).render
    outputElem.appendChild(newOutput)
  })

  // show variables, debug...
  dom.document.addEventListener("eval-var-updated", (e: dom.CustomEvent) => {
    showVariables()
  })

  // get input from user
  dom.document.addEventListener("eval-input", (e: dom.CustomEvent) => {
    val nodeId = e.detail.asDyn.nodeId.toString
    val name = e.detail.asDyn.name.toString

    val valueInputElem = input().render
    val valueBtnElem = button("Enter").render
    val enterValueDiv = div(
      label(
        s"Please enter value for '$name': ",
        valueInputElem,
        valueBtnElem
      )
    ).render
    outputElem.appendChild(enterValueDiv)

    valueInputElem.focus()

    valueBtnElem.onclick = _ => {
      val inputValue = valueInputElem.value.trim
      val key = SymbolKey(name, Symbol.Kind.Variable)
      val sym = interpreter.symTab.getSymbol(null, key)
      try {
        val value = sym.tpe.get match
          case Expression.Type.Integer  => inputValue.toInt
          case Expression.Type.Real     => inputValue.toDouble
          case Expression.Type.Boolean  => inputValue.toBoolean
          case Expression.Type.String   => inputValue
        interpreter.symTab.setValue(nodeId, name, value)
        interpreter.continue()

        val newOutput = pre(s"Please enter value for '$name': $inputValue").render
        outputElem.removeChild(enterValueDiv)
        outputElem.appendChild(newOutput)
      } catch {
        case (e: EvalException) => // from symbol table
          displayError(e.getMessage)
        case e: (NumberFormatException | IllegalArgumentException) =>
          displayError(s"Entered invalid ${sym.tpe.get}: '${inputValue}'")
      }
    }
  })

  def displayError(msg: String): Unit =
    outputElem.innerText = msg
    outputElem.classList.add("error")

  def showVariables(): Unit =
    variablesElem.innerText = ""
    val varValues = interpreter.symTab.varSymbols
    varValues.foreach { sym =>
      val symElem = div(s"${sym.key.name}: ${sym.tpe.get} = ${sym.value.getOrElse("")}").render
      variablesElem.appendChild(symElem)
    }
}

def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString