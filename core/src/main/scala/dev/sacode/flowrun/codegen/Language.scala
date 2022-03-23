package dev.sacode.flowrun.codegen

enum Language(val name: String, val prism: String):
  case java extends Language("java", "java")
  case javascript extends Language("javascript", "javascript")
  case python extends Language("python", "python")
  case scala extends Language("scala", "scala")
  case c extends Language("c", "c")
  case cPLusPLus extends Language("c++", "cpp")
  case cSharp extends Language("c#", "csharp")
  case kotlin extends Language("kotlin", "kotlin")
  case php extends Language("php", "php")
  case golang extends Language("golang", "golang")
  case swift extends Language("swift", "swift")
  case pascal extends Language("pascal", "pascal")
  case ruby extends Language("ruby", "ruby")
  case rust extends Language("rust", "rust")
