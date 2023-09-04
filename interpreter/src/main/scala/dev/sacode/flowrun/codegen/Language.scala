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
  case swift extends Language("swift", "swift")
  case ruby extends Language("ruby", "ruby")

// umm..
//case golang extends Language("golang", "golang")

// unknown
//case rust extends Language("rust", "rust")

//case nodejs extends Language("nodejs", "javascript")

// complex? variable declarations
// case pascal extends Language("pascal", "pascal")

// complex string handling..
// case c extends Language("c", "c")
