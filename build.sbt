inThisBuild(
  List(
    scalaVersion := "3.6.2",
    evictionErrorLevel := Level.Warn,
    publish / skip := true,
    scalafmtSbt := true,
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    organization := "ba.sake",
    homepage := Some(url("https://github.com/sacode387/FlowRun")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
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
      "com.lihaoyi" %%% "scalatags" % "0.13.1",
      "com.lihaoyi" %%% "utest" % "0.8.5" % Test
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines",
      "128",
      "-Wsafe-init",
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
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
      "com.outr" %%% "reactify" % "4.1.3",
      "ba.sake" %%% "tupson" % "0.13.0",
      "com.lihaoyi" %%% "utest" % "0.8.5" % Test
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines",
      "128",
      "-Wsafe-init",
      "-deprecation",
      "-Yretain-trees"
    ),

    // tests
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / parallelExecution := false,
    Test / jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13)
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    publish / skip := false
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
