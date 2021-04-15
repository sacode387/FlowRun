package ba.sake.flowrun
package eval

import scala.concurrent.ExecutionContext
import utest._

object InterpreterTests extends TestSuite {
  given ExecutionContext = scala.concurrent.ExecutionContext.global

  var stmtCount: Int = 0

  def getId(): String =
    stmtCount += 1
    "stmt-" + stmtCount

  val tests = Tests {
    test("dry run") {
      val main = Function("main", None, List(Statement.Begin, Statement.End))
      val programModel = ProgramModel(Program("program", main))
      val interpreter = Interpreter(programModel)
      interpreter.run().map { _ =>
        assert(1 == 1)
      }
    }

    test("declare") {
      val main = Function("main", None,
        List(
          Statement.Begin,
          Statement.Declare(getId(), "x", Expression.Type.Integer, None),
          Statement.Declare(getId(), "y", Expression.Type.Integer, Some("5")),
          Statement.End
        )
      )
      val programModel = ProgramModel(Program("program", main))
      val interpreter = Interpreter(programModel)
      val symTab = interpreter.symTab

      interpreter.run().map { _ =>
        assert(symTab.isDeclaredVar("x"))
        assert(symTab.isDeclaredVar("y"))
        symTab.getValue("123", "y") ==> 5
      }
    }

    test("assign") {
      val main = Function("main", None,
        List(
          Statement.Begin,
          Statement.Declare(getId(), "x", Expression.Type.Integer, None),
          Statement.Assign(getId(), "x", "5"),
          Statement.End
        )
      )
      val programModel = ProgramModel(Program("program", main))
      val interpreter = Interpreter(programModel)
      val symTab = interpreter.symTab

      interpreter.run().map { _ =>
        assert(symTab.isDeclaredVar("x"))
        symTab.getValue("123", "x") ==> 5
      }
    }

    test("arithmetic") {
      val main = Function("main", None, 
        List(
          Statement.Begin,
          Statement.Declare(getId(), "x", Expression.Type.Integer, Some("5 + 3 * 2")),
          Statement.Declare(getId(), "y", Expression.Type.Integer, Some("15 / 3 - 2")),
          Statement.Declare(getId(), "z", Expression.Type.Integer, Some("15 % 2 + 2")),
          Statement.End
        )
      )
      val programModel = ProgramModel(Program("program", main))
      val interpreter = Interpreter(programModel)
      val symTab = interpreter.symTab

      interpreter.run().map { _ =>
        symTab.getValue("123", "x") ==> 11
        symTab.getValue("123", "y") ==> 3
        symTab.getValue("123", "z") ==> 3
      }
    }

    test("if-else") {
      val main = Function("main", None,
        List(
          Statement.Begin,
          Statement.Declare(getId(), "x", Expression.Type.Integer, None),
          Statement.If(getId(), "true",
            Statement.Block(getId(), List(Statement.Assign(getId(), "x", "1"))), // true
            Statement.Block(getId(), List(Statement.Assign(getId(), "x", "2"))) // false
          ),
          Statement.End
        )
      )
      val programModel = ProgramModel(Program("program", main))
      val interpreter = Interpreter(programModel)
      val symTab = interpreter.symTab

      interpreter.run().map { _ =>
        symTab.getValue("123", "x") ==> 1
      }
    }
  }
}