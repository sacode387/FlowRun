package dev.sacode.flowrun.ast

import java.util.UUID

object AST:
  def newId: String =
    "id_" + UUID.randomUUID.toString.replaceAll("-", "_")