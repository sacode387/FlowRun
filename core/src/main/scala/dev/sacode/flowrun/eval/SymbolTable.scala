package dev.sacode.flowrun
package eval

import scala.collection.mutable
import reactify.*
import dev.sacode.flowrun.Expression.Type

class SymbolTable(flowrunChannel: Channel[FlowRun.Event]) {

  val globalScope = Scope("GLOBAL", None, flowrunChannel)
  locally {
    val key = SymbolKey("abs", Symbol.Kind.Function)
    globalScope.add(null, key, Type.Integer, None)
  }

  private var currentScope = globalScope

  def enterScope(name: String): Unit =
    val newScope = Scope(name, Some(currentScope), flowrunChannel)
    currentScope.childScopes = currentScope.childScopes.appended(newScope)
    currentScope = newScope
    flowrunChannel := FlowRun.Event.SymbolTableUpdated

  def exitScope(): Unit =
    currentScope = currentScope.parentScope.getOrElse(
      throw RuntimeException(s"Cannot exit scope ${currentScope.name}")
    )
    flowrunChannel := FlowRun.Event.SymbolTableUpdated

  def varSymbols: List[Symbol] =
    currentScope.allSymbols.values.filter(_.key.kind == Symbol.Kind.Variable).toList

  def add(nodeId: String, key: SymbolKey, tpe: Type, value: Option[Any]): Symbol =
    currentScope.add(nodeId, key, tpe, value)

  def setValue(nodeId: String, name: String, value: Any): Unit =
    currentScope.setValue(nodeId, name, value)

  def getValue(nodeId: String, name: String): Any =
    currentScope.getValue(nodeId, name)

  def isDeclaredVar(name: String): Boolean =
    currentScope.isDeclaredVar(name)

  def isDeclaredFun(name: String): Boolean =
    currentScope.isDeclaredFun(name)

  def getSymbol(nodeId: String, key: SymbolKey): Symbol =
    currentScope.getSymbol(nodeId, key)

  private def error(msg: String, nodeId: String) =
    throw EvalException(msg, nodeId)
}

/** One scope level:
  *   - global
  *   - function
  *   - block (for loop, while loop)
  */
class Scope(
    val name: String,
    val parentScope: Option[Scope],
    flowrunChannel: Channel[FlowRun.Event]
):

  private var symbols: Map[SymbolKey, Symbol] = Map()

  // for easier testing...
  var childScopes = List.empty[Scope]

  def allSymbols: Map[SymbolKey, Symbol] = symbols

  // we assume type is good here
  def add(nodeId: String, key: SymbolKey, tpe: Type, value: Option[Any]): Symbol =
    if symbols.isDefinedAt(key) then
      error(s"${key.kind.toString} with name '${key.name}' is already declared.", nodeId)
    val newSymbol = Symbol(key, tpe, value, this)
    symbols += (key -> newSymbol)
    flowrunChannel := FlowRun.Event.SymbolTableUpdated
    newSymbol

  def set(key: SymbolKey, newSymbol: Symbol): Unit =
    symbols += (key -> newSymbol)

  def get(key: SymbolKey): Option[Symbol] =
    symbols.get(key)

  def setValue(nodeId: String, name: String, value: Any): Unit =
    val key = SymbolKey(name, Symbol.Kind.Variable)
    val sym = getSymbol(nodeId, key)
    val updateValue = TypeUtils.getUpdateValue(nodeId, name, sym.tpe, value).get
    val updatedSym = sym.copy(value = Some(updateValue))
    sym.scope.set(key, updatedSym)
    flowrunChannel := FlowRun.Event.SymbolTableUpdated

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
    val scopesChain = mutable.ListBuffer.empty[Scope]
    var tmpScope = this
    while tmpScope.parentScope.isDefined do
      scopesChain += tmpScope.parentScope.get
      tmpScope = tmpScope.parentScope.get
    scopesChain.foldLeft(this.get(key)) { (maybeSym, parentScope) =>
      maybeSym.orElse(parentScope.allSymbols.get(key))
    }

  private def error(msg: String, nodeId: String) =
    // TODO val maybeFun = if name.startsWith("fun-") then s" [in function '$name']" else ""
    val maybeFun = s" [in function '$name']"
    throw EvalException(msg + maybeFun, nodeId)
end Scope

case class SymbolKey(name: String, kind: Symbol.Kind)

case class Symbol(key: SymbolKey, tpe: Type, value: Option[Any] = None, scope: Scope):
  override def toString: String =
    s"${key}: ${tpe}"

object Symbol:
  enum Kind:
    case Const, Variable, Function
