package ba.sake.flowrun
package eval

import scala.scalajs.js

import ba.sake.flowrun.Expression.Type

class SymbolTable() {

  var symbols: Map[SymbolKey, Symbol] = Map()

  // we assume type is good here
  def add(nodeId: String, key: SymbolKey, tpe: Type, value: Option[Any]): Symbol =
    if isDeclared(key) then
      error(s"${key.kind.toString} with name '${key.name}' is already declared.", nodeId)
    val newSymbol = Symbol(key, tpe, value)
    symbols += (key -> newSymbol)
    EventUtils.dispatchEvent("eval-var-updated", null)
    newSymbol

  def set(nodeId: String, name: String, value: Any): Unit =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    symbols.get(key) match
      case None =>
        error(s"Variable '$name' is not declared.", nodeId)
      case Some(sym) =>
        val updateValue = TypeUtils.getUpdateValue(nodeId, name, sym.tpe, value)
        val updatedSym = sym.copy(value = Some(updateValue))
        symbols += (key -> updatedSym)
        EventUtils.dispatchEvent("eval-var-updated", null)

  def get(nodeId: String, name: String): Any =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    symbols.get(key) match
      case None =>
        error(s"Variable '$name' is not declared.", nodeId)
      case Some(sym) =>
        sym.value.getOrElse(error(s"Variable '$name' is not initialized.", nodeId))
  
  def isDeclared(key: SymbolKey): Boolean =
    symbols.isDefinedAt(key)

  private def error(msg: String, nodeId: String): Unit =
    throw EvalException(msg, nodeId)
}

case class SymbolKey(name: String, kind: Symbol.Kind)
case class Symbol(key: SymbolKey, tpe: Type, value: Option[Any] = None)

object Symbol:
  enum Kind:
    case Variable, Function
