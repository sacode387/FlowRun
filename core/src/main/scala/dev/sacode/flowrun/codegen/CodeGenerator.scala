package dev.sacode.flowrun.codegen

import scala.util.Try

trait CodeGenerator {

  def generate: Try[CodeGenRes]
}

case class CodeGenRes(
  lines: List[String],
  stmtLineNums: Map[String, List[Int]]
)