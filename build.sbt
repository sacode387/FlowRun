
import org.scalajs.linker.interface.ModuleSplitStyle

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
      "com.lihaoyi" %%% "scalatags" % "0.9.4"
    ).map(_.cross(CrossVersion.for3Use2_13)),
    libraryDependencies ++= Seq(
      //TODO RC2"org.getshaka" %%% "native-converter" % "0.4.0",
      //"com.lihaoyi" %%% "pprint" % "0.6.4",
      
    ),
    scalacOptions ++= Seq(
      "-Xmax-inlines", "64",
      "-Ycheck-init"
    ),
    (Compile / compile) := {
      WebKeys.assets.value // run assets
      (Compile / compile).value
    },
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    scalaJSLinkerOutputDirectory in (Compile, fastLinkJS) :=
      (Assets / WebKeys.public).value / "scripts"
  )
  .enablePlugins(ScalaJSPlugin, SbtWeb)
