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
  val container = document.getElementById("cy")
  val outputElem = document.getElementById("run-output")
  val variablesElem = document.getElementById("variables-output")
  val runBtn = document.getElementById("run-btn").asInstanceOf[dom.html.Button]
  val editWrapperElem = document.getElementById("edit-wrapper")

  val mainFunction = Function("main")
  val program = Program("program", mainFunction)
  val programModel = ProgramModel(program)
  val cytoscapeFlowchart = CytoscapeFlowchart(programModel, container, editWrapperElem)

  var interpreter = Interpreter(programModel)

  var lastRun: String = ""

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
    val valueLabelElem = label(
      s"Please enter value for '$name': ",
      valueInputElem,
      valueBtnElem
    ).render
    outputElem.appendChild(valueLabelElem)

    valueInputElem.focus()

    valueBtnElem.onclick = _ => {
      val inputValue = valueInputElem.value.trim
      val sym = interpreter.symTab.symbols(name)
      try {
        val value = sym.tpe match
          case Expression.Type.Integer  => inputValue.toInt
          case Expression.Type.Real     => inputValue.toDouble
          case Expression.Type.Boolean  => inputValue.toBoolean
          case Expression.Type.String   => inputValue
        interpreter.symTab.set(nodeId, name, value)
        interpreter.continue()

        val newOutput = pre(s"Please enter value for '$name': $inputValue").render
        outputElem.removeChild(valueLabelElem)
        outputElem.appendChild(newOutput)
      } catch {
        case (e: EvalException) => // from symbol table
          displayError(e.getMessage)
        case e: (NumberFormatException | IllegalArgumentException) =>
          displayError(s"Entered invalid ${sym.tpe}: '${inputValue}'")
      }
    }
  })

  def displayError(msg: String): Unit =
    outputElem.innerText = msg
    outputElem.classList.add("error")

  def showVariables(): Unit =
    variablesElem.innerText = ""
    interpreter.symTab.symbols.values.foreach { sym =>
      val symElem = div(s"${sym.name}: ${sym.tpe} = ${sym.value.getOrElse("")}").render
      variablesElem.appendChild(symElem)
    }
}

def getNowTime: String =
  val now = new js.Date()
  now.toLocaleTimeString