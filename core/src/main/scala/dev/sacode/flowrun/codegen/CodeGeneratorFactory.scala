package dev.sacode.flowrun.codegen

import dev.sacode.flowrun.ast.Program

object CodeGeneratorFactory {

  def apply(lang: Language, programAst: Program): CodeGenerator =
    lang match
      case Language.scala => ScalaGenerator(programAst)
      case Language.java  => JavaGenerator(programAst)

}
