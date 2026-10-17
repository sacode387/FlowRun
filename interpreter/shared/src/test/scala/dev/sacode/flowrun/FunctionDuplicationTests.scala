package dev.sacode.flowrun

import ba.sake.tupson.*
import reactify.*
import utest.*
import dev.sacode.flowrun.ast.*
import Statement.*

object FunctionDuplicationTests extends TestSuite {
  private def ids(s: Statement): List[String] = s.id :: (s match {
    case s: Block   => s.statements.flatMap(ids)
    case s: If      => ids(s.trueBlock) ++ ids(s.falseBlock)
    case s: While   => ids(s.body)
    case s: DoWhile => ids(s.body)
    case s: ForLoop => ids(s.body)
    case _          => Nil
  })
  private def ids(f: Function): List[String] = f.id :: (f.parameters.map(_.id) ++ f.statements.flatMap(ids))

  val tests = Tests {
    test("a copied function executes after JSON round-trip") {
      import scala.concurrent.ExecutionContext.Implicits.global
      val source = Function(
        "f",
        "f",
        List(Function.Parameter("x", "x", Expression.Type.Integer)),
        Expression.Type.Integer,
        List(Begin("fb"), Return("fr", Some("x + 1")))
      )
      val main = Function("main", "main", statements = List(Output("out", "fCopy(7)", true)))
      val channel = Channel[FlowRun.Event]
      val model = ProgramModel(Program("p", "p", FlowRunConfig.default, main, List(source)), channel)
      model.duplicateFunction(source.id)
      val restored = ProgramModel(model.ast.toJson.parseJson[Program], channel)
      var output = ""
      channel.attach { case FlowRun.Event.EvalOutput(text, _) => output = text; case _ => () }
      val interpreter = eval.Interpreter(restored, channel, Fixtures.setInterval, Fixtures.clearInterval)
      interpreter.run().map { _ => output ==> "8" }
    }

    test("deep copies use fresh IDs, unique names, preserve signatures and calls") {
      val source = Function(
        "f",
        "calculate",
        List(Function.Parameter("param", "x", Expression.Type.Integer)),
        Expression.Type.Integer,
        List(
          Begin("begin"),
          If(
            "if",
            "true",
            Block(
              "yes",
              List(
                While(
                  "while",
                  "false",
                  Block(
                    "wb",
                    List(
                      DoWhile(
                        "do",
                        "false",
                        Block(
                          "db",
                          List(ForLoop("for", "i", "0", "1", "1", Block("forb", List(Call("call", "calculate(x)")))))
                        )
                      )
                    )
                  )
                )
              )
            ),
            Block("no")
          ),
          Return("return", Some("x"))
        )
      )
      val main = Function("main", "main")
      val channel = Channel[FlowRun.Event]
      var updates = 0
      channel.attach { case FlowRun.Event.FunctionUpdated => updates += 1; case _ => () }
      val model = ProgramModel(Program("p", "p", FlowRunConfig.default, main, List(source)), channel)
      model.duplicateFunction(source.id)
      val copy = model.currentFunction
      model.duplicateFunction(source.id)
      model.currentFunction.name ==> "calculateCopy2"
      copy.name ==> "calculateCopy"
      copy.tpe ==> source.tpe
      copy.parameters.map(_.name) ==> List("x")
      val allIds = model.ast.allFunctions.flatMap(ids)
      assert(allIds.distinct.size == allIds.size)
      copy.statements.last.asInstanceOf[Return].maybeValue ==> Some("x")
      val restored = model.ast.toJson.parseJson[Program]
      restored ==> model.ast
      assert(restored.toJson.contains("calculate(x)"))
      model.ast.revision ==> 3
      updates ==> 2
      model.currentSelectedFunctionId = copy.id
      model.delete(copy.statements.head.id)
      model.deleteFunction(copy.id)
      model.ast.functions.head ==> source
      assertThrows[IllegalArgumentException] { model.duplicateFunction(main.id) }
    }
  }
}
