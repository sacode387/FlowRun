package ba.sake.flowrun
package eval

import scala.scalajs.js

import ba.sake.flowrun.Expression.Type

// TODO function scoped
class SymbolTable() {
  private var scopes = List(Scope())

  private def currentScope =
    scopes.head

  def enterScope(): Unit =
    scopes = scopes.prepended(Scope())
    EventUtils.dispatchEvent("eval-var-updated", null)
  
  def exitScope(): Unit =
    scopes = scopes.tail
    EventUtils.dispatchEvent("eval-var-updated", null)

  def varSymbols: List[Symbol] =
    currentScope.symbols.values.filter(_.key.kind == Symbol.Kind.Variable).toList

  def add(nodeId: String, key: SymbolKey, tpe: Option[Type], value: Option[Any]): Symbol =
    currentScope.add(nodeId, key, tpe, value)

  def setValue(nodeId: String, name: String, value: Any): Unit =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    val sym = getSymbol(nodeId, key)
    val updateValue = TypeUtils.getUpdateValue(nodeId, name, sym.tpe.get, value)
    val updatedSym = sym.copy(value = Some(updateValue))
    sym.scope.symbols += (key -> updatedSym)
    EventUtils.dispatchEvent("eval-var-updated", null)

  def getValue(nodeId: String, name: String): Any =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    val sym = getSymbol(nodeId, key)
    sym.value.getOrElse(error(s"Variable '$name' is not initialized.", nodeId))

  def isDeclaredVar(name: String): Boolean =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    maybeSymbol(key).nonEmpty
  
  def isDeclaredFun(name: String): Boolean =
    val key = SymbolKey(name, Symbol.Kind.Function)
    maybeSymbol(key).nonEmpty
  
  def getSymbol(nodeId: String, key: SymbolKey): Symbol =
    maybeSymbol(key) match
      case None =>
        error(s"${key.kind} '${key.name}' is not declared.", nodeId)
      case Some(sym) =>
        sym

  private def maybeSymbol(key: SymbolKey): Option[Symbol] =
    maybeScope(key).map(_.symbols(key))

  private def maybeScope(key: SymbolKey): Option[Scope] =
    scopes.find(s => s.symbols.get(key).isDefined)

  private def error(msg: String, nodeId: String): Nothing =
    throw EvalException(msg, nodeId)
}


class Scope() {

  var symbols: Map[SymbolKey, Symbol] = Map()

  // we assume type is good here
  def add(nodeId: String, key: SymbolKey, tpe: Option[Type], value: Option[Any]): Symbol =
    if symbols.isDefinedAt(key) then
      error(s"${key.kind.toString} with name '${key.name}' is already declared.", nodeId)
    val newSymbol = Symbol(key, tpe, value, this)
    symbols += (key -> newSymbol)
    EventUtils.dispatchEvent("eval-var-updated", null)
    newSymbol

  private def error(msg: String, nodeId: String): Nothing =
    throw EvalException(msg, nodeId)
}

case class SymbolKey(name: String, kind: Symbol.Kind)
case class Symbol(key: SymbolKey, tpe: Option[Type], value: Option[Any] = None, scope: Scope)

object Symbol:
  enum Kind:
    case Const, Variable, Function
