package ba.sake.flowrun
package eval

import scalajs.js.annotation._
import ba.sake.flowrun.Expression.Type

@JSExportTopLevel("SymbolTable", Module.Exec)
class SymbolTable() {

  private var symbols: Map[String, Symbol] = Map()

  def add(name: String, tpe: Type, kind: Symbol.Kind, value: Option[Any]): Symbol = {
    var newSymbol = Symbol(name, tpe, kind)
    value.foreach(v => newSymbol = newSymbol.copy(value = value))
    symbols += (name -> newSymbol)
    println(s"SYMBOLS: $symbols")
    newSymbol
  }

  def set(nodeId: String, name: String, value: Any): Unit = {
    symbols.get(name) match {
      case None => throw EvalException(s"Variable '$name' is not defined.", nodeId)
      case Some(sym) =>
        val updatedSym = sym.copy(value = Some(value))
        symbols += (name -> updatedSym)
    }
  }

  def get(nodeId: String, name: String): Any =
    symbols.get(name) match {
      case None => throw EvalException(s"Variable '$name' is not defined.", nodeId)
      case Some(sym) => sym.value.getOrElse(throw EvalException(s"Variable '$name' is not initialized.", nodeId))
    }

}


case class Symbol(name: String, tpe: Type, kind: Symbol.Kind, value: Option[Any] = None)

object Symbol {
  enum Kind {
    case Var, Arg
  }
}