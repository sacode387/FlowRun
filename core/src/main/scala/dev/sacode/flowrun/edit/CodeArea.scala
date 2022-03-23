package dev.sacode.flowrun.edit

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

  def render(config: FlowRunConfig): Unit = {
    var prismLang = config.lang.prism

    val codeElem = code(cls := s"language-$prismLang")(
      codeText(config)
    ).render

    flowRunElements.codeArea.innerText = ""
    flowRunElements.codeArea.appendChild(codeElem)
    js.Dynamic.global.Prism.highlightElement(codeElem)
  }

  def codeText(config: FlowRunConfig): String =
    val generator = CodeGeneratorFactory(config.lang, programModel.ast)
    val codeTry = generator.generate
    if codeTry.isFailure then println("Failed to generate code: " + codeTry.failed)
    codeTry.getOrElse("Error while generating code:\n" + codeTry.failed.get.getMessage)

}
