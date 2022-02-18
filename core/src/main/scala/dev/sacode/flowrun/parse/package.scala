package dev.sacode.flowrun.parse

import dev.sacode.flowrun.ast.Expression

def parseExpr(nodeId: String, str: String): Expression =
  val tokens = Lexer(nodeId, str).lex()
  ExpressionParser(nodeId, tokens).parse()
