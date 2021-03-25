package ba.sake.flowrun.parse

import scala.collection.mutable

final class Lexer(nodeId: String, input: String) {
  import Token.Type

  def lex(): List[Token] =
    val tokens = mutable.ArrayBuffer.empty[Token]
    var i = 0
    while i < input.length do
      val pos = i
      val lookahead = input(i)
      if lookahead.isWhitespace then
        i += 1 // ignore ws
      else if lookahead == '(' then
        i += 1
        tokens += Token(Type.LeftParen, lookahead.toString, pos)
      else if lookahead == ')' then
        i += 1
        tokens += Token(Type.RightParen, lookahead.toString, pos)
      else if lookahead == '+' then
        i += 1
        tokens += Token(Type.Plus, lookahead.toString, pos)
      else if lookahead == '-' then
        i += 1
        tokens += Token(Type.Minus, lookahead.toString, pos)
      else if lookahead == '*' then
        i += 1
        tokens += Token(Type.Times, lookahead.toString, pos)
      else if lookahead == '/' then
        i += 1
        tokens += Token(Type.Div, lookahead.toString, pos)
      else if lookahead == '%' then
        i += 1
        tokens += Token(Type.Mod, lookahead.toString, pos)
      else if lookahead == '!' then
        if i+1 < input.length && input(i+1) == '=' then
          tokens += Token(Type.NotEquals, "!=", pos)
          i += 2
        else
          tokens += Token(Type.Not, lookahead.toString, pos)
          i += 1
      else if lookahead == '<' then
        if i+1 < input.length && input(i+1) == '=' then
          tokens += Token(Type.LtEq, "<=", pos)
          i += 2
        else
          tokens += Token(Type.Lt, lookahead.toString, pos)
          i += 1
      else if lookahead == '>' then
        if i+1 < input.length && input(i+1) == '=' then
          tokens += Token(Type.GtEq, ">=", pos)
          i += 2
        else
          tokens += Token(Type.Gt, lookahead.toString, pos)
          i += 1
      else if lookahead == '=' then
        if i+1 < input.length && input(i+1) == '=' then
          tokens += Token(Type.EqualsEquals, "==", pos)
          i += 2
        else
          error(s"Expected '==' but found '='", i)
      else if lookahead == '&' then
        if i+1 < input.length && input(i+1) == '&' then
          tokens += Token(Type.And, "&&", pos)
          i += 2
        else
          error(s"Expected '&&' but found '&'", i)
      else if lookahead == '|' then
        if i+1 < input.length && input(i+1) == '|' then
          tokens += Token(Type.Or, "||", pos)
          i += 2
        else
          error(s"Expected '||' but found '|'", i)
      else if lookahead.isDigit then
        var text = ""
        while i < input.length && input(i).isDigit do
          text += input(i)
          i += 1
        if i < input.length && input(i) == '.' then
          text += input(i)
          i += 1
          if i < input.length && !input(i).isDigit
          then error(s"""Expected digits but found '${input(i)}'""", i)
          while i < input.length && input(i).isDigit do
            text += input(i)
            i += 1
          tokens += Token(Type.Real, text, pos)
        else
          tokens += Token(Type.Integer, text, pos)
      else if lookahead.isLetter then
        var text = ""
        while i < input.length && input(i).isLetterOrDigit do
          text += input(i)
          i += 1
        val token = text match
          case "true"   => Token(Type.True, "true", pos)
          case "false"  => Token(Type.False, "false", pos)
          case "null"   => Token(Type.Null, "null", pos)
          case _        => Token(Type.Identifier, text, pos)
        tokens += token
      else if input(i) == '"' then
        i += 1
        var text = ""
        while i < input.length && input(i) != '"' do
          text += input(i)
          i += 1
        if i == input.length
        then error("""Unclosed string. Expected '"'""", i)
        else tokens += Token(Type.String, text, pos)
        i += 1 // once more for closing "
      else
        error(s"Unknown character '$lookahead'", pos)

    tokens += Token(Type.EOF, "<EOF>", i) // special end marker
    tokens.toList
  end lex

  private def error(msg: String, pos: Int): Unit =
    val where = if pos >= input.length-1 then "end of input" else s"position $pos"
    println((pos, input.length))
    throw LexException(s"$msg at $where", nodeId)
}

case class Token(
  tpe: Token.Type,
  text: String,
  pos: Int
)

object Token {
  enum Type {
    case Integer    // 123
    case Real       // 123.45
    case String     // "abc"
    case Identifier // abc

    case True
    case False

    case Null

    case Plus
    case Minus
    case Times
    case Div
    case Mod

    case Gt
    case GtEq
    case Lt
    case LtEq
    case EqualsEquals
    case Not
    case NotEquals
    case And
    case Or

    case LeftParen
    case RightParen

    case EOF
  }
}

class LexException(msg: String, nodeId: String) extends RuntimeException(msg)
