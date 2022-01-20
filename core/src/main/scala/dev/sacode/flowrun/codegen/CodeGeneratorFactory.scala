package dev.sacode.flowrun.codegen

import dev.sacode.flowrun.Program

object CodeGeneratorFactory {

  def apply(lang: Language, programAst: Program): CodeGenerator =
    lang match
      case Language.scala2 => Scala2Generator(programAst)
      case Language.java   => JavaGenerator(programAst)

}
