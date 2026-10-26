package dev.sacode.flowrun.formatgen

import utest.*
import dev.sacode.flowrun.*
import dev.sacode.flowrun.ast.*
import Statement.*

object DotGeneratorTests extends TestSuite {
  private def chart(stmts: Statement*): String =
    DotGenerator(
      Function("main", "main", statements = Begin("begin") :: stmts.toList ::: List(Return("return"))),
      FlowRunTheme.default
    ).generate

  // Read the emitted geometry and edit targets, rather than whitespace or colors.
  private def positions(dot: String): Map[String, (Double, Double)] =
    dot.linesIterator
      .flatMap("""^\s*(\w+) \[.*?pos="([^,]+),([^!]+)!".*$""".r.findFirstMatchIn)
      .map(m => m.group(1) -> (m.group(2).toDouble, m.group(3).toDouble))
      .toMap

  private def point(x: Int, y: Int): (Double, Double) = (x * 1.25, 10000 - y * 0.9375)

  private def branch(id: String, yes: List[Statement] = Nil, no: List[Statement] = Nil): If =
    If(id, "x > 0", Block(s"${id}t", yes), Block(s"${id}f", no))

  private def editIds(dot: String): Set[String] =
    """id="([^"]+@[^"]+)""".r.findAllMatchIn(dot).map(_.group(1)).toSet

  private def chain(onTrue: Boolean, body: List[Statement] = List(Output("chosen", "x", true))): If = {
    def link(id: String, yes: List[Statement], no: List[Statement]): If =
      if onTrue then branch(id, no, yes) else branch(id, yes, no)
    link("a", body, List(link("b", Nil, List(link("c", Nil, Nil)))))
  }

  private def checkInsertionTargets(root: If, dot: String): Unit = {
    // Exercise the same edge ID decoding as CtxMenu, then insert through the real model.
    for id <- editIds(dot) do {
      val Array(after, block) = id.split("@")
      val model = ProgramModel(
        Program(
          "p",
          "p",
          FlowRunConfig.default,
          Function("main", "main", statements = List(Begin("begin"), root, Return("return")))
        ),
        reactify.Channel[FlowRun.Event]
      )
      model.addStmt(Comment("inserted", "new"), block, after.stripPrefix("end_"))
      val statements =
        if block == "fun-main" then model.currentFunction.statements
        else {
          val parent = model.findStatement(block.dropRight(1)).asInstanceOf[If]
          (if block.endsWith("t") then parent.trueBlock else parent.falseBlock).statements
        }
      val index = statements.indexWhere(_.id == after.stripPrefix("end_"))
      statements(index + 1).id ==> "inserted"
    }
  }

  val tests = Tests {
    test("diagram defaults soften the palette without changing semantic shapes") {
      val dot = chart(Assign("assign", "x", "1"), Input("input", "x", None), branch("if"))

      assert(
        dot.contains("node [penwidth=1.2"),
        dot.contains("style=\"filled\""),
        dot.contains("edge [penwidth=1.35"),
        dot.contains("color=\"#475569\""),
        dot.contains("assign [id=\"assign#Assign\""),
        dot.contains("shape=\"invtrapezium\""),
        dot.contains("shape=\"diamond\"")
      )
    }

    test("ordinary decisions retain symmetric branches, merge and insertion targets") {
      val dot = chart(branch("a", List(Assign("yes", "x", "1")), List(Output("no", "x", true))))
      val p = positions(dot)
      p("a") ==> point(0, 1)
      p("yes") ==> point(1, 2)
      p("no") ==> point(-1, 2)
      p("end_a") ==> point(0, 3)
      p("return") ==> point(0, 4)
      editIds(dot) ==> Set("begin@fun-main", "a@at", "a@af", "yes@at", "no@af", "end_a@fun-main")
      assert(dot.contains("taillabel=\"true\""), dot.contains("taillabel=\"false\""))
    }

    test("empty decisions and long labels reserve existing space") {
      val empty = positions(chart(branch("a")))
      empty("end_a") ==> point(0, 2)
      val wide = positions(chart(branch("a").copy(condition = "x" * 40)))
      wide("true_dummy_up_a") ==> point(3, 1)
      wide("false_dummy_up_a") ==> point(-3, 1)
    }

    test("single decisions on both sides align with their parent and retain separate merges") {
      val dot = chart(branch("a", List(branch("b")), List(branch("c"))))
      val p = positions(dot)
      p("b") ==> point(1, 1)
      p("c") ==> point(-1, 1)
      p("end_b") ==> point(1, 2)
      p("end_c") ==> point(-1, 2)
      p("end_a") ==> point(0, 2)
      p("return") ==> point(0, 3)
      editIds(dot) ==> Set(
        "begin@fun-main",
        "a@at",
        "a@af",
        "b@bt",
        "b@bf",
        "c@ct",
        "c@cf",
        "end_b@at",
        "end_c@af",
        "end_a@fun-main"
      )
      assert(
        dot.contains("a:e -> b:w"),
        dot.contains("a:w -> c:e"),
        dot.contains("end_b:w -> end_a:e"),
        dot.contains("end_c:e -> end_a:w")
      )
      checkInsertionTargets(branch("a", List(branch("b")), List(branch("c"))), dot)
    }

    test("while, for and do-while retain body, return routing and following positions") {
      val body = Block("body", List(Assign("inc", "x", "x+1")))
      for loop <- List[Statement](While("loop", "true", body), ForLoop("loop", "i", "0", "1", "5", body)) do {
        val dot = chart(loop)
        val p = positions(dot)
        p("loop") ==> point(0, 1)
        p("inc") ==> point(1, 2)
        p("end_dummy_down_loop") ==> point(0, 3)
        p("return") ==> point(0, 4)
        editIds(dot) ==> Set("begin@fun-main", "loop@body", "inc@body", "loop@fun-main")
      }
      val dot = chart(DoWhile("loop", "true", body))
      val p = positions(dot)
      p("end_loop") ==> point(0, 1)
      p("inc") ==> point(0, 2)
      p("loop") ==> point(0, 3)
      p("return") ==> point(0, 4)
      assert(dot.contains("begin:s -> end_loop:n"))
      editIds(dot) ==> Set("begin@fun-main", "loop@body", "inc@body", "loop@fun-main")
    }

    test("straight chains align conditions and preserve every editable interval in either direction") {
      for onTrue <- List(false, true) do {
        val root = chain(onTrue)
        val dot = chart(root)
        val p = positions(dot)
        p("a")._2 ==> p("b")._2
        p("b")._2 ==> p("c")._2
        math.abs(p("b")._1 - p("a")._1) ==> 1.25
        math.abs(p("c")._1 - p("b")._1) ==> 1.25
        assert(
          if onTrue then p("a")._1 < p("b")._1 && p("b")._1 < p("c")._1
          else p("a")._1 > p("b")._1 && p("b")._1 > p("c")._1
        )
        p("end_c")._2 ==> p("end_b")._2
        p("end_b")._2 ==> p("end_a")._2
        assert(p("end_a")._2 > p("return")._2)
        val continuing = if onTrue then "t" else "f"
        val outcome = if onTrue then "f" else "t"
        editIds(dot) ==> Set(
          "begin@fun-main",
          "a@at",
          "a@af",
          "b@bt",
          "b@bf",
          "c@ct",
          "c@cf",
          s"chosen@a$outcome",
          s"end_b@a$continuing",
          s"end_c@b$continuing",
          "end_a@fun-main"
        )
        val side = if onTrue then "e" else "w"
        val entry = if onTrue then "w" else "e"
        assert(dot.contains(s"a:$side -> b:$entry [id=\"a@a$continuing\""))
        assert(dot.contains(s"taillabel=\"$onTrue\""))

        checkInsertionTargets(root, dot)
      }
    }

    test("both branches allow unequal bodies and outward chains without overlapping the parent") {
      val yes = branch("b", yes = List(branch("d")), no = List(Output("wide", "x" * 60, true)))
      val no = branch("c", yes = List(While("loop", "true", Block("body"))))
      val p = positions(chart(branch("a", List(yes), List(no))))
      p("a")._2 ==> p("b")._2
      p("a")._2 ==> p("c")._2
      p("b")._2 ==> p("d")._2
      assert(
        p("c")._1 < p("a")._1,
        p("a")._1 < p("b")._1,
        p("b")._1 < p("d")._1,
        p("end_a")._2 < p("end_dummy_down_loop")._2
      )
      p("end_a")._2 ==> p("end_b")._2
      p("end_a")._2 ==> p("end_c")._2
      p("end_a")._2 ==> p("end_d")._2
    }

    test("long conditions grow the column spacing to fit their rendered widths") {
      val child = branch("b").copy(condition = "x" * 18)
      val root = branch("a", no = List(child)).copy(condition = "x" * 18)
      val p = positions(chart(root))
      // Each diamond is 2.48 inches wide, plus clearance between them.
      assert(math.abs(p("b")._1 - p("a")._1) >= 2.48 + 18.0 / 96)
    }

    test("adding another statement and alternating directions retain ordinary nesting") {
      val original = chain(false)
      val extra = original.copy(falseBlock =
        original.falseBlock.copy(statements = Comment("before", "keep") :: original.falseBlock.statements)
      )
      val p = positions(chart(extra))
      assert(p("a")._2 > p("b")._2)
      val alternate = branch("a", no = List(branch("b", yes = List(branch("c")))))
      val q = positions(chart(alternate))
      assert(q("a")._2 > q("b")._2, q("b")._2 > q("c")._2)
    }

    test("long outcomes and loops reserve room for neighboring lanes") {
      val body = List(While("loop", "true", Block("body", List(Output("wide", "x" * 90, true)))))
      val root = branch(
        "outer",
        List(Comment("lead", "before choices"), chain(false, body)),
        List(Output("neighbor", "x", true))
      )
      val p = positions(chart(root))
      assert(
        p("neighbor")._1 < p("choice_dummy_up_c")._1,
        p("end_a")._2 < p("end_dummy_down_loop")._2,
        p("end_outer")._2 < p("end_a")._2,
        p("return")._2 < p("end_outer")._2
      )
    }
  }
}
