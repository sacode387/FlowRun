import org.scalajs.linker.interface.OutputPatterns

inThisBuild(
  List(
    scalaVersion := "3.2.2",
    evictionErrorLevel := Level.Warn,
    publish / skip := true,
    scalafmtSbt := true,
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    resolvers += "jitpack" at "https://jitpack.io",
    organization := "dev.sacode",
    licenses := List("GPL-3.0" -> url("https://www.gnu.org/licenses/gpl-3.0.html")),
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

lazy val core = (project in file("core"))
  .settings(
    name := "FlowRun",
    libraryDependencies ++= Seq(
      ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),
      "io.github.cquiroz" %%% "scala-java-time" % "2.4.0-M1",
      "com.lihaoyi" %%% "scalatags" % "0.11.0",
      "com.outr" %%% "reactify" % "4.0.6",
      "ba.sake" %%% "tupson" % "0.5.1",
      "com.lihaoyi" %%% "utest" % "0.7.10" % Test,
      "com.lihaoyi" %%% "pprint" % "0.7.1" % Test
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines",
      "64",
      "-Ysafe-init",
      "-deprecation",
      "-Yretain-trees"
    ),
    //scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },

    // tests
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / parallelExecution := false,
    //Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) },
    Test / jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .enablePlugins(ScalaJSPlugin)

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
  .dependsOn(core)
  .enablePlugins(ScalaJSPlugin, SbtWeb)
