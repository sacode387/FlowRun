ThisBuild / evictionErrorLevel := Level.Warn

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

// for tests
libraryDependencies += ("org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-web" % "1.4.4")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
