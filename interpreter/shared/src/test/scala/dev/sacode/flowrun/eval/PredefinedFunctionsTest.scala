package dev.sacode.flowrun
package eval

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import reactify.*
import utest.*
import dev.sacode.flowrun.ast.*
import dev.sacode.flowrun.eval.RunVal.*

object PredefinedFunctionsTest extends TestSuite {

  given ExecutionContext = scala.concurrent.ExecutionContext.global

  val S = Statement
  val E = Expression

  val tests = Tests {

    test("Abs") {
      val main = newFun(List())
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p1", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)
      val intAssertions = Seq((5, 5), (-0, 0), (-7, 7))
        .map((n, e) => (RunVal.IntegerVal(n), RunVal.IntegerVal(e)))
      val realAssertions = Seq((5.5, 5.5), (-0.0, 0.0), (-7.7, 7.7))
        .map((n, e) => (RunVal.RealVal(n), RunVal.RealVal(e)))
      val allAssertions = intAssertions ++ realAssertions
      Future.traverse(allAssertions) { (n, expected) =>
        interpreter.evalPredefinedFunction("", PredefinedFunction.Abs, Seq(n)).map { res =>
          assert(res == expected)
        }
      }
    }

    test("Floor") {
      val main = newFun(List())
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p1", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)
      val realAssertions = Seq((5.5, 5.0), (0.0, 0.0), (-7.7, -8.0))
        .map((n, e) => (RunVal.RealVal(n), RunVal.RealVal(e)))
      val allAssertions = realAssertions
      Future.traverse(allAssertions) { (n, expected) =>
        interpreter.evalPredefinedFunction("", PredefinedFunction.Floor, Seq(n)).map { res =>
          assert(res == expected)
        }
      }
    }

    test("Ceil") {
      val main = newFun(List())
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p1", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)
      val realAssertions = Seq((5.5, 6.0), (0.0, 0.0), (-7.7, -7.0))
        .map((n, e) => (RunVal.RealVal(n), RunVal.RealVal(e)))
      val allAssertions = realAssertions
      Future.traverse(allAssertions) { (n, expected) =>
        interpreter.evalPredefinedFunction("", PredefinedFunction.Ceil, Seq(n)).map { res =>
          assert(res == expected)
        }
      }
    }

    test("Length") {
      val main = newFun(List())
      val flowrunChannel = Channel[FlowRun.Event]
      val programModel = ProgramModel(Program("p1", "program", FlowRunConfig.default, main), flowrunChannel)
      val interpreter = Interpreter(programModel, flowrunChannel)
      val realAssertions = Seq(("", 0), ("abc", 3), ("     ", 5))
        .map((n, e) => (RunVal.StringVal(n), RunVal.IntegerVal(e)))
      val allAssertions = realAssertions
      Future.traverse(allAssertions) { (n, expected) =>
        interpreter.evalPredefinedFunction("", PredefinedFunction.Length, Seq(n)).map { res =>
          assert(res == expected)
        }
      }
    }
  }

  def newFun(statements: List[Statement]) = {
    val stmts = List(S.Begin(AST.newId)) ++ statements ++ List(S.Return(AST.newId))
    Function("main", "main", statements = stmts)
  }
}
