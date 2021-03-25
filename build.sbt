
import org.scalajs.linker.interface.ModuleSplitStyle

inThisBuild(
  List(
    scalaVersion := "3.0.0-RC1"
  )
)

lazy val core = (project in file("core"))
  .settings(
    name := "FlowRun",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
        ("org.scala-js" %%% "scalajs-dom" % "1.1.0").withDottyCompat(scalaVersion.value),
        ("com.lihaoyi" %%% "pprint" % "0.6.2")
    ),
    (Compile / compile) := {
      WebKeys.assets.value // run assets
      ( Compile / compile).value
    },
    // https://stackoverflow.com/a/29375359/4496364
    (Compile / fastOptJS / artifactPath) :=
      (Assets / WebKeys.public).value / "scripts" / ((moduleName in fastOptJS).value + "-fastopt.js"),
  )
  .enablePlugins(ScalaJSPlugin, SbtWeb)

