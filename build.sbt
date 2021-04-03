
import org.scalajs.linker.interface.ModuleSplitStyle

inThisBuild(
  List(
    scalaVersion := "3.0.0-RC1"
  )
)

lazy val core = (project in file("core"))
  .settings(
    name := "FlowRun",
    libraryDependencies ++= Seq(
        ("org.scala-js" %%% "scalajs-dom" % "1.1.0").withDottyCompat(scalaVersion.value),
        "org.getshaka" %%% "native-converter" % "0.4.0",
        "com.lihaoyi" %%% "pprint" % "0.6.2",
    ),
    scalacOptions ++= Seq("-Ycheck-init"),
    (Compile / compile) := {
      WebKeys.assets.value // run assets
      ( Compile / compile).value
    },
    scalaJSLinkerConfig ~= {
      .withModuleKind(ModuleKind.ESModule)
    },
    scalaJSLinkerOutputDirectory in (Compile, fastLinkJS) :=
      (Assets / WebKeys.public).value / "scripts"
  )
  .enablePlugins(ScalaJSPlugin, SbtWeb)
