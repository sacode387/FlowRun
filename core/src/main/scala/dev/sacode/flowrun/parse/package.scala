package dev.sacode.flowrun
package parse

def parseExpr(nodeId: String, str: String): Expression =
  val tokens = Lexer(nodeId, str).lex()
  ExpressionParser(nodeId, tokens).parse()
