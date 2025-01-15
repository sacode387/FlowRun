ThisBuild / evictionErrorLevel := Level.Warn

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

// for tests
libraryDependencies += ("org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.1")

addSbtPlugin("com.github.sbt" % "sbt-web" % "1.5.8")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.3")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.9.2")
