package dev.sacode.flowrun

import dev.sacode.flowrun.ast.Function

object FlowRun:
  enum Event:
    case EvalSuccess
    case EvalBeforeExecStatement
    case EvalAfterExecStatement
    case EvalError(nodeId: String, msg: String, funId: String)
    case EvalOutput(msg: String, newline: Boolean)
    case EvalInput(nodeId: String, name: String, prompt: Option[String])
    case ClearOutput()
    case EvalFunctionStarted
    case EvalFunctionFinished
    case SyntaxSuccess
    case StmtDeleted
    case StmtAdded
    case StmtUpdated(nodeId: String)
    case SyntaxError(msg: String)
    case SymbolTableUpdated
    case FunctionUpdated
    case FunctionSelected
    case StmtSelected
    case Deselected
    case ConfigChanged
    case SvgMounted

extension (str: String) {

  def toIdentifier: String =
    str.replaceAll("\\s", "")

  def indented(x: Int): String =
    val spaces = " " * x
    str.linesIterator
      .map { line =>
        spaces + line
      }
      .mkString("\n")
}
