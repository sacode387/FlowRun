package dev.sacode.flowrun.parse

import utest.*

object LexerTests extends TestSuite {

  val tests = Tests {
    test("all tokens") {
      val str = """
      123
      3.14
      "abc"
      xyz
      true
      false
      +
      -
      *
      /
      %
      >
      >=
      <
      <=
      ==
      !
      !=
      &&
      ||
      (
      )
      ,

      """
      val tokens = Lexer("123", str).lex()
      val tokenTypes = tokens.map(_.tpe)
      assert(tokenTypes == Token.Type.values.toList)
    }
  }
}
