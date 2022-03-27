package dev.sacode.flowrun.codegen

enum Language(val name: String, val prism: String):
  case java extends Language("java", "java")
  case javascript extends Language("javascript", "javascript")
  case python extends Language("python", "python")
  case scala extends Language("scala", "scala")
  case cSharp extends Language("c#", "csharp")
  case cPLusPLus extends Language("c++", "cpp")
  case kotlin extends Language("kotlin", "kotlin")
  case php extends Language("php", "php")
  case golang extends Language("golang", "golang")
  case swift extends Language("swift", "swift")
  case rust extends Language("rust", "rust")
  case ruby extends Language("ruby", "ruby")
  case nodejs extends Language("nodejs", "javascript")
  case pascal extends Language("pascal", "pascal")
 // case c extends Language("c", "c")
