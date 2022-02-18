package dev.sacode.flowrun
package eval

import scala.concurrent.ExecutionContext
import reactify.*
import utest.*
import dev.sacode.flowrun.ast.*

object InterpreterTests extends TestSuite {
  given ExecutionContext = scala.concurrent.ExecutionContext.global

  val S = Statement
  val E = Expression

  var stmtCount: Int = 0

  def getId(): String =
    stmtCount += 1
    s"stmt-$stmtCount"

  val tests = Tests {

    test("dry run") {
      val main = newFun(List())
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p1", "program", main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)
      interpreter.run().map { _ =>
        assert(1 == 1)
      }
    }

    test("declare uninitialized") {
      val main = newFun(
        List(
          S.Declare(getId(), "empty", E.Type.Integer, None)
        )
      )

      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p2", "program", main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        assert(!scope.isDeclaredVar("nope"))
        assert(scope.isDeclaredVar("empty"))
        intercept[EvalException](scope.getValue("", "nope"))
        intercept[EvalException](scope.getValue("", "empty"))
      }
    }

    test("declare initialized") {
      val main = newFun(
        List(
          S.Declare(getId(), "integer", E.Type.Integer, Some("5")),
          S.Declare(getId(), "neg_integer", E.Type.Integer, Some("-123")),
          S.Declare(getId(), "real", E.Type.Real, Some("12.345")),
          S.Declare(getId(), "string", E.Type.String, Some("\"abc\"")),
          S.Declare(getId(), "boolean", E.Type.Boolean, Some("false")),
          S.Declare(getId(), "neg_boolean", E.Type.Boolean, Some("!false"))
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p2", "program", main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head

        scope.getValue("", "integer") ==> 5
        scope.getValue("", "neg_integer") ==> -123

        scope.getValue("", "real") ==> 12.345
        scope.getValue("", "string") ==> "abc"
        scope.getValue("", "boolean") ==> false
        scope.getValue("", "neg_boolean") ==> true
      }
    }

    test("assign") {
      val main = newFun(
        List(
          S.Declare(getId(), "x", E.Type.Integer, None),
          S.Assign(getId(), "x", "6"),
          S.Output(getId(), "x")
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p3", "program", main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        assert(scope.isDeclaredVar("x"))
        scope.getValue("123", "x") ==> 6
      }
    }

    test("arithmetic") {
      val main = newFun(
        List(
          S.Declare(getId(), "a", E.Type.Integer, Some("5 + 3 * 2")),
          S.Declare(getId(), "b", E.Type.Integer, Some("15 / 3 - 2")),
          S.Declare(getId(), "c", E.Type.Integer, Some("15 % (2 + 2)")),
          S.Declare(getId(), "d", E.Type.Boolean, Some("1 == 1")),
          S.Declare(getId(), "e", E.Type.Boolean, Some("2 >= 1")),
          S.Declare(getId(), "f", E.Type.Boolean, Some("2 <= 1"))
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p4", "program", main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        scope.getValue("", "a") ==> 11
        scope.getValue("", "b") ==> 3
        scope.getValue("", "c") ==> 3
        scope.getValue("", "d") ==> true
        scope.getValue("", "e") ==> true
        scope.getValue("", "f") ==> false
      }
    }

    test("if-else") {
      val main = newFun(
        List(
          S.Declare(getId(), "x", E.Type.Integer, None),
          S.If(
            getId(),
            "true",
            S.Block(getId(), List(S.Assign(getId(), "x", "1"))), // true
            S.Block(getId(), List(S.Assign(getId(), "x", "2"))) // false
          )
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p5", "program", main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        scope.getValue("123", "x") ==> 1
      }
    }
  }

  def newFun(statements: List[Statement]) = {
    val stmts = List(S.Begin(AST.newId)) ++ statements ++ List(S.Return(AST.newId))
    Function("main", "main", statements = stmts)
  }
}
