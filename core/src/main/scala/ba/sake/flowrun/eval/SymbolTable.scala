package ba.sake.flowrun
package eval

import scala.scalajs.js

import ba.sake.flowrun.Expression.Type

// TODO function scoped
class SymbolTable() {

  var symbols: Map[SymbolKey, Symbol] = Map()

  // we assume type is good here
  def add(nodeId: String, key: SymbolKey, tpe: Option[Type], value: Option[Any]): Symbol =
    if symbols.isDefinedAt(key) then
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
        val updateValue = TypeUtils.getUpdateValue(nodeId, name, sym.tpe.get, value)
        val updatedSym = sym.copy(value = Some(updateValue))
        symbols += (key -> updatedSym)
        EventUtils.dispatchEvent("eval-var-updated", null)

  def getValue(nodeId: String, name: String): Any =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    symbols.get(key) match
      case None =>
        error(s"Variable '$name' is not declared.", nodeId)
      case Some(sym) =>
        sym.value.getOrElse(error(s"Variable '$name' is not initialized.", nodeId))
  
  def get(nodeId: String, key: SymbolKey): Symbol =
    symbols.get(key) match
      case None =>
        error(s"Symbol '${key.name}' is not declared.", nodeId)
      case Some(sym) =>
        sym
  
  def isDeclaredVar(name: String): Boolean =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    symbols.isDefinedAt(key)
  
  def isDeclaredFun(name: String): Boolean =
    val key = SymbolKey(name, Symbol.Kind.Function)
    symbols.isDefinedAt(key)

  private def error(msg: String, nodeId: String): Nothing =
    throw EvalException(msg, nodeId)
}

case class SymbolKey(name: String, kind: Symbol.Kind)
case class Symbol(key: SymbolKey, tpe: Option[Type], value: Option[Any] = None)

object Symbol:
  enum Kind:
    case Variable, Function
