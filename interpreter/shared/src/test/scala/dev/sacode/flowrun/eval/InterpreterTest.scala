package dev.sacode.flowrun
package eval

import scala.concurrent.ExecutionContext
import reactify.*
import utest.*
import dev.sacode.flowrun.ast.*
import dev.sacode.flowrun.eval.RunVal.*

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
      val programModel = ProgramModel(Program("p1", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel, Fixtures.setInterval, Fixtures.clearInterval)
      interpreter.run().map { _ =>
        assert(1 == 1)
      }
    }

    test("declare uninitialized") {
      val main = newFun(
        List(
          S.Declare(getId(), "empty", E.Type.Integer, None,1)
        )
      )

      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p2", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel, Fixtures.setInterval, Fixtures.clearInterval)

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
          S.Declare(getId(), "integer", E.Type.Integer, Some("5"),1),
          S.Declare(getId(), "neg_integer", E.Type.Integer, Some("-123"),1),
          S.Declare(getId(), "real", E.Type.Real, Some("12.345"),1),
          S.Declare(getId(), "string", E.Type.String, Some("\"abc\""),1),
          S.Declare(getId(), "boolean", E.Type.Boolean, Some("false"),1),
          S.Declare(getId(), "neg_boolean", E.Type.Boolean, Some("!false"),1)
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p2", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel, Fixtures.setInterval, Fixtures.clearInterval)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head

        scope.getValue("", "integer") ==> IntegerVal(5)
        scope.getValue("", "neg_integer") ==> IntegerVal(-123)

        scope.getValue("", "real") ==> RealVal(12.345)
        scope.getValue("", "string") ==> StringVal("abc")
        scope.getValue("", "boolean") ==> BooleanVal(false)
        scope.getValue("", "neg_boolean") ==> BooleanVal(true)
      }
    }

    test("assign") {
      val main = newFun(
        List(
          S.Declare(getId(), "x", E.Type.Integer, None,1),
          S.Assign(getId(), "x", "6"),
          S.Output(getId(), "x", true)
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p3", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel, Fixtures.setInterval, Fixtures.clearInterval)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        assert(scope.isDeclaredVar("x"))
        scope.getValue("123", "x") ==> IntegerVal(6)
      }
    }

    test("arithmetic - integers") {
      val main = newFun(
        List(
          S.Declare(getId(), "a", E.Type.Integer, Some("5 + 3 * 2"),1),
          S.Declare(getId(), "b", E.Type.Integer, Some("15 / 3 - 2"),1),
          S.Declare(getId(), "c", E.Type.Integer, Some("15 % (2 + 2)"),1),
          S.Declare(getId(), "d", E.Type.Boolean, Some("1 == 1"),1),
          S.Declare(getId(), "e", E.Type.Boolean, Some("2 >= 1"),1),
          S.Declare(getId(), "f", E.Type.Boolean, Some("2 <= 1"),1)
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p4", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel, Fixtures.setInterval, Fixtures.clearInterval)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        scope.getValue("", "a") ==> IntegerVal(11)
        scope.getValue("", "b") ==> IntegerVal(3)
        scope.getValue("", "c") ==> IntegerVal(3)
        scope.getValue("", "d") ==> BooleanVal(true)
        scope.getValue("", "e") ==> BooleanVal(true)
        scope.getValue("", "f") ==> BooleanVal(false)
      }
    }

    test("arithmetic - reals") {
      val main = newFun(
        List(
          S.Declare(getId(), "a", E.Type.Real, Some("5.1 + 3.0 * 2.0"),1),
          S.Declare(getId(), "b", E.Type.Real, Some("15.0 / 3.0 - 2.5"),1),
          S.Declare(getId(), "c", E.Type.Real, Some("15.0 % 4.5"),1),
          S.Declare(getId(), "d", E.Type.Boolean, Some("1.0 == 1.0"),1),
          S.Declare(getId(), "e", E.Type.Boolean, Some("2.0 >= 1.0"),1),
          S.Declare(getId(), "f", E.Type.Boolean, Some("2.0 <= 1.0"),1),
          S.Declare(getId(), "g", E.Type.Real, Some("5.0 / 2.0"),1)
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p4", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel, Fixtures.setInterval, Fixtures.clearInterval)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        scope.getValue("", "a") ==> RealVal(11.1)
        scope.getValue("", "b") ==> RealVal(2.5)
        scope.getValue("", "c") ==> RealVal(1.5)
        scope.getValue("", "g") ==> RealVal(2.5)

        scope.getValue("", "d") ==> BooleanVal(true)
        scope.getValue("", "e") ==> BooleanVal(true)
        scope.getValue("", "f") ==> BooleanVal(false)
      }
    }

    test("if-else") {
      val main = newFun(
        List(
          S.Declare(getId(), "x", E.Type.Integer, None,1),
          S.If(
            getId(),
            "true",
            S.Block(getId(), List(S.Assign(getId(), "x", "1"))), // true
            S.Block(getId(), List(S.Assign(getId(), "x", "2"))) // false
          )
        )
      )
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p5", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel, Fixtures.setInterval, Fixtures.clearInterval)

      interpreter.run().map { _ =>
        val scope = interpreter.symTab.globalScope.childScopes.head
        scope.getValue("123", "x") ==> IntegerVal(1)
      }
    }
  }

  def newFun(statements: List[Statement]) = {
    val stmts = List(S.Begin(AST.newId)) ++ statements ++ List(S.Return(AST.newId))
    Function("main", "main", statements = stmts)
  }
}
