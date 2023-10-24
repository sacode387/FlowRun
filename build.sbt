import org.scalajs.linker.interface.OutputPatterns

inThisBuild(
  List(
    scalaVersion := "3.3.1",
    evictionErrorLevel := Level.Warn,
    publish / skip := true,
    scalafmtSbt := true,
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    organization := "dev.sacode",
    licenses := List(      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/sacode387/FlowRun"),
        "scm:git:git@github.com:sacode387/FlowRun.git"
      )
    ),
    developers := List(
      Developer(
        "sake92",
        "Sakib Hadžiavdić",
        "sakib@sake.ba",
        url("https://sake.ba")
      )
    )
  )
)

lazy val editor = (project in file("editor"))
  .settings(
    name := "flowrun-editor",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.12.0",
      "com.lihaoyi" %%% "utest" % "0.8.1" % Test,
      "com.lihaoyi" %%% "pprint" % "0.8.1" % Test
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines",
      "128",
      "-Ysafe-init",
      "-deprecation",
      "-Yretain-trees"
    ),

    // tests
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / parallelExecution := false,
    Test / jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .dependsOn(interpreter.js)
  .enablePlugins(ScalaJSPlugin)

lazy val interpreter = crossProject(JVMPlatform, JSPlatform)
  .in(file("interpreter"))
  .settings(
    name := "flowrun-interpreter",
    publish / skip := false,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "com.outr" %%% "reactify" % "4.0.8",
      "ba.sake" %%% "tupson" % "0.7.0",
      "com.lihaoyi" %%% "utest" % "0.8.1" % Test,
      "com.lihaoyi" %%% "pprint" % "0.8.1" % Test
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines",
      "128",
      "-Ysafe-init",
      "-deprecation",
      "-Yretain-trees"
    ),

    // tests
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / parallelExecution := false,
    Test / jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),
  )

lazy val demo = (project in file("demo"))
  .settings(
    scalaJSUseMainModuleInitializer := true,
    (Compile / compile) := {
      WebKeys.assets.value
      (Compile / compile).value
    },
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory :=
      (Assets / WebKeys.public).value / "scripts"
  )
  .dependsOn(editor)
  .enablePlugins(ScalaJSPlugin, SbtWeb)
