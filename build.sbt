
import org.scalajs.linker.interface.OutputPatterns

inThisBuild(
  List(
    scalaVersion := "3.0.0-RC3"
  )
)

lazy val core = (project in file("core"))
  .settings(
    name := "FlowRun",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.1.0",
      "com.lihaoyi" %%% "scalatags" % "0.9.4",
      "com.outr" %%% "reactify" % "4.0.4",
      "com.lihaoyi" %%% "pprint" % "0.6.4"
    ).map(_.cross(CrossVersion.for3Use2_13)),
    libraryDependencies ++= Seq(
      "org.getshaka" %%% "native-converter" % "0.4.1",
      "com.lihaoyi" %%% "utest" % "0.7.9" % Test
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines", "64",
      "-Ysafe-init",
      "-deprecation"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },

    // tests
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / parallelExecution := false,
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) },
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