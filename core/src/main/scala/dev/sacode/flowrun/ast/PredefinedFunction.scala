package dev.sacode.flowrun.ast

enum PredefinedFunction(val name: String) {
  // numbers
  case Abs extends PredefinedFunction("abs")
  // strings
  case Length extends PredefinedFunction("length")
  case CharAt extends PredefinedFunction("charAt")
}

object PredefinedFunction {
  def withName(name: String): Option[PredefinedFunction] =
    PredefinedFunction.values.find(_.name == name)
}
