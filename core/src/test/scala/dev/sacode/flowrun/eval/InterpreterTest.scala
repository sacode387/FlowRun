package dev.sacode.flowrun
package eval

import scala.concurrent.ExecutionContext
import reactify.*
import utest.*

object InterpreterTests extends TestSuite {
  given ExecutionContext = scala.concurrent.ExecutionContext.global

  var stmtCount: Int = 0

  def getId(): String =
    stmtCount += 1
    s"stmt-$stmtCount"

  val tests = Tests {
    
    test("dry run") {
      val main = Function("main", "main")
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p1", "program", main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)
      interpreter.run().map { _ =>
        assert(1 == 1)
      }
    }

    test("declare uninitialized") {
      val main = Function("main", "main", statements =
        List(
          Statement.Declare(getId(), "empty", Expression.Type.Integer, None)
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
      val main = Function("main", "main", statements =
        List(
          Statement.Declare(getId(), "integer", Expression.Type.Integer, Some("5")),
          Statement.Declare(getId(), "neg_integer", Expression.Type.Integer, Some("-123")),
          Statement.Declare(getId(), "real", Expression.Type.Real, Some("12.345")),
          Statement.Declare(getId(), "string", Expression.Type.String, Some("\"abc\"")),
          Statement.Declare(getId(), "boolean", Expression.Type.Boolean, Some("false")),
          Statement.Declare(getId(), "neg_boolean", Expression.Type.Boolean, Some("!false")),
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
      val main = Function("main", "main", statements =
        List(
          Statement.Declare(getId(), "x", Expression.Type.Integer, None),
          Statement.Assign(getId(), "x", "6"),
          Statement.Output(getId(), "x")
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
      val main = Function("main", "main", statements =
        List(
          Statement.Declare(getId(), "a", Expression.Type.Integer, Some("5 + 3 * 2")),
          Statement.Declare(getId(), "b", Expression.Type.Integer, Some("15 / 3 - 2")),
          Statement.Declare(getId(), "c", Expression.Type.Integer, Some("15 % (2 + 2)")),
          Statement.Declare(getId(), "d", Expression.Type.Boolean, Some("1 == 1")),
          Statement.Declare(getId(), "e", Expression.Type.Boolean, Some("2 >= 1")),
          Statement.Declare(getId(), "f", Expression.Type.Boolean, Some("2 <= 1")),
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
      val main = Function("main", "main", statements =
        List(
          Statement.Declare(getId(), "x", Expression.Type.Integer, None),
          Statement.If(getId(), "true",
            Statement.Block(getId(), List(Statement.Assign(getId(), "x", "1"))), // true
            Statement.Block(getId(), List(Statement.Assign(getId(), "x", "2"))) // false
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
}