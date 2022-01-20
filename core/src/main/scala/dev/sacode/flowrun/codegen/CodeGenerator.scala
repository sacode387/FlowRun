package dev.sacode.flowrun.codegen

import scala.util.Try

trait CodeGenerator {

  def generate: Try[String]
}
