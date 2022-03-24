package dev.sacode.flowrun.edit

import scala.util.{Success, Failure}
import reactify.*
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import scalatags.JsDom.all.*
import dev.sacode.flowrun.codegen.CodeGeneratorFactory
import dev.sacode.flowrun.FlowRunConfig
import dev.sacode.flowrun.ProgramModel
import dev.sacode.flowrun.FlowRunElements
import dev.sacode.flowrun.codegen.Language

class CodeArea(
    flowRunElements: FlowRunElements,
    programModel: ProgramModel,
    config: Var[FlowRunConfig]
) {

  def init(): Unit = {
    Language.values.foreach { language =>
      val item = option(value := language.name)(language.name).render
      flowRunElements.codeLang.add(item)
    }
    flowRunElements.codeLang.value = config.lang.name
    flowRunElements.codeLang.onchange = { (e: dom.Event) =>
      config.set {
        config().copy(lang = Language.values.find(_.name == flowRunElements.codeLang.value).get)
      }
    }
  }

  def render(stmtId: String): Unit = {

    val (text, lh) = gen(stmtId)
    val codeElem =
      code(cls := s"language-${config.lang.prism}")(
        text
      ).render

    flowRunElements.codeArea.dataset("line") = lh
    flowRunElements.codeArea.innerText = ""
    flowRunElements.codeArea.appendChild(codeElem)
    //js.Dynamic.global.Prism.highlightElement(codeElem)
    js.Dynamic.global.Prism.highlightAll()
  }

  def codeText(): String =
    val (text, _) = gen("")
    text

  private def gen(stmtId: String): (String, String) = {
    val generator = CodeGeneratorFactory(config().lang, programModel.ast)
    val codeTry = generator.generate
    if codeTry.isFailure then println("Failed to generate code: " + codeTry.failed)
    codeTry match {
      case Success(res) =>
        val lh = res.stmtLineNums.getOrElse(stmtId, List.empty).mkString(",")
        (res.lines.mkString("\n"), lh)
      case Failure(_) =>
        ("Error while generating code:\n" + codeTry.failed.get.getMessage, "")
    }
  }
}
