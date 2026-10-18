package dev.sacode.flowrun

import ba.sake.tupson.*
import reactify.*
import utest.*
import dev.sacode.flowrun.ast.*
import Statement.*

object EditingHistoryTests extends TestSuite {
  private val nested = If("if", "true", Block("yes", List(Output("out", "1", true))), Block("no"))
  private val fun = Function("f", "f", statements = List(Begin("fb"), Return("fr")))
  private val program = Program(
    "p", "Original", FlowRunConfig.default,
    Function("main", "main", statements = List(Begin("b"), nested, Return("r"))), List(fun)
  )
  private def content(p: Program): Program = p.copy(revision = 0)
  private def model = ProgramModel(program, Channel[FlowRun.Event])

  val tests = Tests {
    test("nested edits round-trip with monotonic revisions and notifications") {
      val channel = Channel[FlowRun.Event]
      var restored = 0
      channel.attach { case FlowRun.Event.ProgramReplaced => restored += 1; case _ => () }
      val m = ProgramModel(program, channel)
      m.currentSelectedStmtId = Some("out")
      m.updateStmt(Output("out", "2", true))
      m.addStmt(Comment("comment", "hello"), "yes", "out")
      m.delete("if")
      m.undo()
      m.currentSelectedStmtId ==> Some("out")
      m.findStatement("comment") ==> Comment("comment", "hello")
      m.undo()
      m.undo()
      content(m.ast) ==> content(program)
      assert(!m.canUndo, m.canRedo)
      m.redo(); m.redo(); m.redo()
      assert(!m.canRedo)
      m.ast.main.statements ==> List(Begin("b"), Return("r"))
      m.ast.revision ==> 10
      restored ==> 6
      m.ast.toJson.parseJson[Program] ==> m.ast
    }
    test("function add duplicate signature and delete restore valid selections") {
      val m = model
      m.currentSelectedFunctionId = fun.id
      m.currentSelectedStmtId = Some("fb")
      m.duplicateFunction(fun.id)
      val copy = m.currentFunction
      m.currentSelectedStmtId = Some(copy.statements.head.id)
      val param = Function.Parameter("param", "x", Expression.Type.Integer)
      m.updateFunction(copy.statements.head.id, name = Some("renamed"), parameters = Some(List(param)))
      m.deleteFunction(copy.id)
      m.undo()
      m.currentFunction.name ==> "renamed"
      m.currentFunction.parameters ==> List(param)
      m.currentSelectedStmtId ==> Some(copy.statements.head.id)
      m.undo(); m.undo()
      m.currentSelectedFunctionId ==> fun.id
      m.currentSelectedStmtId ==> Some("fb")
      content(m.ast) ==> content(program)
      m.addFunction()
      val added = m.currentFunction
      m.undo(); m.redo()
      m.currentFunction ==> added
    }
    test("name config and import are undoable including identity") {
      val m = model
      m.setName("New name")
      m.setConfig(m.ast.config.copy(lang = "python", showGenCode = false))
      val beforeImport = m.ast
      val imported = program.copy(id = "imported", name = "Imported", functions = Nil, revision = 500)
      m.replaceProgram(imported)
      m.currentSelectedFunctionId ==> ProgramModel.MainFunId
      m.currentSelectedStmtId ==> None
      m.ast.revision ==> 501
      m.undo()
      content(m.ast) ==> content(beforeImport)
      m.ast.revision ==> 502
      m.undo(); m.undo()
      content(m.ast) ==> content(program)
      m.redo(); m.redo(); m.redo()
      content(m.ast) ==> content(imported)
    }
    test("typing groups end on blur or structural edits and ignore no-ops") {
      val m = model
      m.recordTextEdit(); m.setName("N")
      m.recordTextEdit(); m.setName("Ne")
      m.recordTextEdit(); m.setName("New")
      m.endEditGroup()
      m.recordTextEdit(); m.setName("Newer")
      m.endEditGroup()
      m.undo()
      m.ast.name ==> "New"
      m.undo()
      m.ast.name ==> "Original"
      m.redo()
      m.setName("New") // no-op must preserve redo
      assert(m.canRedo)
      m.setConfig(m.ast.config.copy(showIoBtns = false))
      assert(!m.canRedo)
      m.recordTextEdit(); m.setName("Next")
      m.undo()
      m.ast.name ==> "New"
      assert(!m.ast.config.showIoBtns)
    }
    test("selection is not an edit and history is bounded") {
      val m = model
      m.currentSelectedFunctionId = fun.id
      m.currentSelectedStmtId = Some("fb")
      assert(!m.canUndo)
      m.undo(); m.redo()
      m.ast.revision ==> 1
      for i <- 1 to 110 do m.setName(s"Name $i")
      var count = 0
      while m.canUndo do { m.undo(); count += 1 }
      count ==> 100
      m.ast.name ==> "Name 10"
      for i <- 1 to 100 do m.redo()
      m.ast.name ==> "Name 110"
    }
  }
}
