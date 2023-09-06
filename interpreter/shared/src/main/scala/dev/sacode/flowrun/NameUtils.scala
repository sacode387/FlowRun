package dev.sacode.flowrun

import dev.sacode.flowrun.eval.SymbolKey

object NameUtils:
  private val IdentMaxChars = 30
  def validateIdentifier(identifier: String): Option[String] =
    val ident = identifier.trim
    if ident.isEmpty then Some("Name must not be empty")
    else if ident.length > IdentMaxChars then Some(s"Name can be longer than $IdentMaxChars characters")
    else if !ident.head.isLetter then Some("Name must start with a letter")
    else if ident.matches(".*\\s.*") then Some("Name must not contain spaces")
    else if !ident.matches("[a-zA-Z0-9_]+") then Some("Name must contain only letters, numbers or underscore.")
    else if SymbolKey.ReservedWords(ident) then Some("Name must not be a reserved word")
    else None
