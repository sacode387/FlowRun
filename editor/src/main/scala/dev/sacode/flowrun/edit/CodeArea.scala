package dev.sacode.flowrun.edit

import scala.util.{Success, Failure}
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import scalatags.JsDom.all.*
import dev.sacode.flowrun.codegen.CodeGeneratorFactory
import dev.sacode.flowrun.ast.FlowRunConfig
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.FlowRunElements
import dev.sacode.flowrun.codegen.Language

class CodeArea(
    flowRunElements: FlowRunElements,
    programModel: ProgramModel
) {

  def init(): Unit = {
    Language.values.foreach { language =>
      val item = option(value := language.name)(language.name).render
      flowRunElements.codeLang.add(item)
    }
    flowRunElements.codeLang.value = programModel.ast.config.lang
    flowRunElements.codeLang.onchange = { (e: dom.Event) =>
      val oldConfig = programModel.ast.config
      val newConfig = oldConfig.copy(
        lang = flowRunElements.codeLang.value
      )
      programModel.setConfig(newConfig)
    }
  }

  def render(stmtId: String): Unit = {

    val (text, lh) = gen(stmtId)
    val language = resolveLang(programModel.ast.config.lang)
    val codeElem =
      code(cls := s"language-${language.prism}")(
        text
      ).render

    flowRunElements.codeArea.dataset("line") = lh
    flowRunElements.codeArea.innerText = ""
    flowRunElements.codeArea.appendChild(codeElem)
    js.Dynamic.global.Prism.highlightElement(codeElem)
  }

  def codeText(): String =
    val (text, _) = gen("")
    text

  private def gen(stmtId: String): (String, String) = {
    val language = resolveLang(programModel.ast.config.lang)
    val generator = CodeGeneratorFactory(language, programModel.ast)
    val codeTry = generator.generate
    if codeTry.isFailure then codeTry.failed.get.printStackTrace()
    codeTry match {
      case Success(res) =>
        val lh = res.stmtLineNums.getOrElse(stmtId, List.empty).mkString(",")
        (res.lines.mkString("\n"), lh)
      case Failure(_) =>
        ("Error while generating code:\n" + codeTry.failed.get.getMessage, "")
    }
  }

  private def resolveLang(lang: String): Language =
    Language.values.find(_.name == lang).get
}
