package dev.sacode.flowrun.eval

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.*
import reactify.*
import utest.*
import dev.sacode.flowrun.*
import dev.sacode.flowrun.ast.*
import Statement.*
import Interpreter.*

object BreakpointTests extends TestSuite {
  // Drive the real polling boundary manually, with bounded waits for asynchronous evaluation.
  private class Driver(program: Program, breakpoints: Set[String]) {
    private val polls = new LinkedBlockingQueue[() => Unit]()
    private val channel = Channel[FlowRun.Event]
    var output = List.empty[String]
    channel.attach {
      case FlowRun.Event.EvalOutput(text, _) => output = output :+ text
      case _                                 => ()
    }
    val interpreter = Interpreter(
      ProgramModel(program, channel),
      channel,
      (_, body) => { polls.add(() => body); () },
      _ => (),
      breakpoints
    )
    val result = interpreter.run(ExecMode.DEBUG)
    def boundary(id: String, paused: Boolean, step: Boolean = false): Unit = {
      val poll = polls.poll(5, TimeUnit.SECONDS)
      assert(poll != null)
      interpreter.nextExecStatementId ==> Some(id)
      if paused then {
        poll()
        assert(!result.isCompleted)
        assert(polls.isEmpty)
      }
      interpreter.execMode = if step then ExecMode.STEP_BY_STEP else ExecMode.DEBUG
      interpreter.pauseBeforeNext = false
      poll()
    }
    def submitInput(id: String, name: String, value: String): Unit = {
      val poll = polls.poll(5, TimeUnit.SECONDS)
      assert(poll != null)
      interpreter.state ==> State.WAITING_FOR_INPUT
      poll()
      assert(polls.isEmpty)
      Await.result(interpreter.setInputtedValue(id, name, value), 5.seconds)
      poll()
    }
    def finish(): Unit = Await.result(result, 5.seconds)
  }

  val tests = Tests {
    test("breakpoints in called functions rearm in loops; Step then Continue") {
      val fun = Function("f", "f", statements = List(Begin("fb"), Output("out", "i", true), Return("fr")))
      val main = Function(
        "main",
        "main",
        statements = List(
          Begin("b"),
          ForLoop("loop", "i", "0", "1", "1", Block("body", List(Call("call", "f()")))),
          Return("r")
        )
      )
      val d = new Driver(Program("p", "p", FlowRunConfig.default, main, List(fun)), Set("out"))
      d.boundary("b", false)
      d.boundary("loop", false)
      d.boundary("call", false)
      d.boundary("fb", false)
      d.output ==> Nil
      d.boundary("out", true, step = true)
      d.boundary("fr", true)
      d.output ==> List("0")
      d.boundary("loop", false)
      d.boundary("call", false)
      d.boundary("fb", false)
      d.boundary("out", true)
      d.boundary("fr", false)
      d.boundary("loop", false)
      d.boundary("r", false)
      d.finish()
      d.output ==> List("0", "1")
    }
    test("Input pauses before requesting a value and resumes after submission") {
      val main = Function(
        "main",
        "main",
        statements = List(
          Declare("decl", "x", Expression.Type.Integer, None),
          Input("input", "x", None),
          Output("out", "x", true),
          Return("r")
        )
      )
      val d = new Driver(Program("p", "p", FlowRunConfig.default, main), Set("input", "out"))
      d.boundary("decl", false)
      d.interpreter.state ==> State.RUNNING
      d.boundary("input", true)
      d.submitInput("input", "x", "7")
      d.boundary("out", true)
      d.boundary("r", false)
      d.finish()
      d.output ==> List("7")
    }
    test("Stop releases a breakpoint wait") {
      val main = Function("main", "main", statements = List(Output("out", "1", true)))
      val d = new Driver(Program("p", "p", FlowRunConfig.default, main), Set("out"))
      // Stop before the pending timer is polled.
      d.interpreter.state = State.FINISHED_STOPPED
      assertThrows[StoppedException] { d.boundary("out", false); d.finish() }
      d.output ==> Nil
    }
  }
}
