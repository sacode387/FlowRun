package dev.sacode.flowrun.codegen

import dev.sacode.flowrun.ast.Program
import scala.util.Try

object CodeGeneratorFactory {

  def apply(lang: Language, programAst: Program): CodeGenerator =
    lang match
      case Language.scala      => ScalaGenerator(programAst)
      case Language.java       => JavaGenerator(programAst)
      case Language.javascript => JavascriptGenerator(programAst)
      case Language.python     => PythonGenerator(programAst)
      case Language.cSharp     => CSharpGenerator(programAst)
      case Language.cPLusPLus  => CPlusPlusGenerator(programAst)
      case _ =>
        new CodeGenerator {

          def programAst: Program = null

          def generate: Try[CodeGenRes] = Try {
            throw new RuntimeException("TODO")
          }

          override def predefFun(name: String, genArgs: List[String]): String = ""
          override def funCall(name: String, genArgs: List[String]): String = ""
        }

}
