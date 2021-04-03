package ba.sake.flowrun
package eval

import scala.scalajs.js

import ba.sake.flowrun.Expression.Type

class SymbolTable() {

  var symbols: Map[String, Symbol] = Map()

  def add(nodeId: String, name: String, tpe: Type, kind: Symbol.Kind, value: Option[Any]): Symbol = {
    if isDeclared(name) then
      error(s"Variable with name '$name' is already declared.", nodeId)
    var newSymbol = Symbol(name, tpe, kind)
    value.foreach(v => newSymbol = newSymbol.copy(value = value))
    symbols += (name -> newSymbol)
    //println(s"SYMBOLS: $symbols")
    EventUtils.dispatchEvent("eval-var-updated", null)
    newSymbol
  }

  def set(nodeId: String, name: String, value: Any): Unit = {
    symbols.get(name) match {
      case None => error(s"Variable '$name' is not declared.", nodeId)
      case Some(sym) =>
        val updatedSym = sym.copy(value = Some(value))
        symbols += (name -> updatedSym)
        EventUtils.dispatchEvent("eval-var-updated", null)
    }
  }

  def get(nodeId: String, name: String): Any =
    symbols.get(name) match {
      case None => error(s"Variable '$name' is not declared.", nodeId)
      case Some(sym) => sym.value.getOrElse(error(s"Variable '$name' is not initialized.", nodeId))
    }
  
  def isDeclared(name: String): Boolean =
    symbols.isDefinedAt(name)
  
  private def error(msg: String, nodeId: String): Unit =
    throw EvalException(msg, nodeId)
}


case class Symbol(name: String, tpe: Type, kind: Symbol.Kind, value: Option[Any] = None)

object Symbol {
  enum Kind {
    case Var, Arg
  }
}