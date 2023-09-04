package dev.sacode.flowrun.ast

enum PredefinedFunction(val name: String) {
  // numbers
  case Abs extends PredefinedFunction("abs")
  case Floor extends PredefinedFunction("floor")
  case Ceil extends PredefinedFunction("ceil")
  case RandomInteger extends PredefinedFunction("randomInt")
  case Sin extends PredefinedFunction("sin")
  case Cos extends PredefinedFunction("cos")
  case Tan extends PredefinedFunction("tan")
  case Ln extends PredefinedFunction("ln")
  case Log10 extends PredefinedFunction("log")
  case Log2 extends PredefinedFunction("log2")
  // strings
  case Length extends PredefinedFunction("length")
  case CharAt extends PredefinedFunction("charAt")
  // conversions
  case RealToInteger extends PredefinedFunction("real2int")
  case StringToInteger extends PredefinedFunction("string2int")
}

object PredefinedFunction {
  def withName(name: String): Option[PredefinedFunction] =
    PredefinedFunction.values.find(_.name == name)
}
