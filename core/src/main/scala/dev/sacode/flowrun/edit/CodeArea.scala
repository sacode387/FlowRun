package dev.sacode.flowrun.edit

import scala.util.{Success, Failure}
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import scalatags.JsDom.all.*
import dev.sacode.flowrun.codegen.CodeGeneratorFactory
import dev.sacode.flowrun.FlowRunConfig
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.FlowRunElements

class CodeArea(
    flowRunElements: FlowRunElements,
    programModel: ProgramModel
) {

  def render(config: FlowRunConfig, stmtId: String): Unit = {

    val (text, lh) = gen(config, stmtId)

    val codeElem =
      code(cls := s"language-${config.lang.prism}")(
        text
      ).render
    
    println(codeElem.innerHTML)

    flowRunElements.codeArea.dataset("line") = lh
    flowRunElements.codeArea.innerText = ""
    flowRunElements.codeArea.appendChild(codeElem)
    js.Dynamic.global.Prism.highlightElement(codeElem)
  }

  def codeText(config: FlowRunConfig): String =
    val (text, _) = gen(config, "")
    text

  private def gen(config: FlowRunConfig, stmtId: String): (String, String) = {
    val generator = CodeGeneratorFactory(config.lang, programModel.ast)
    val codeTry = generator.generate
    if codeTry.isFailure then println("Failed to generate code: " + codeTry.failed)
    codeTry match {
      case Success(res) =>
        val lh = res.stmtLineNums.getOrElse(stmtId, List.empty).mkString(",")
        println(stmtId)
        println(res.stmtLineNums)
        (res.lines.mkString("\n"), lh)
      case Failure(_) =>
        ("Error while generating code:\n" + codeTry.failed.get.getMessage, "")
    }
  }
}
