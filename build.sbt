
import org.scalajs.linker.interface.OutputPatterns

inThisBuild(
  List(
    scalaVersion := "3.0.0-RC2"
  )
)

lazy val core = (project in file("core"))
  .settings(
    name := "FlowRun",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.1.0",
      "com.lihaoyi" %%% "scalatags" % "0.9.4",
      "com.lihaoyi" %%% "pprint" % "0.6.4"
    ).map(_.cross(CrossVersion.for3Use2_13)),
    libraryDependencies ++= Seq(
      //"org.getshaka" %%% "native-converter" % "0.4.0",
      
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines", "64",
      "-Ycheck-init"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    (Compile / compile) := {
      WebKeys.assets.value // run assets
      (Compile / compile).value
    },
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory :=
      (Assets / WebKeys.public).value / "scripts",

    // tests stuff
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "utest" % "0.7.8" % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) },
    Test / jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .enablePlugins(ScalaJSPlugin, SbtWeb)
