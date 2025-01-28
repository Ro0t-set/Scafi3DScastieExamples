ThisBuild / version := "0.1.0-SNAPSHOT"

scalaVersion := "3.3.4"

enablePlugins(ScalaJSPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "Scafi3DScastieExample"
  )

val scafiVersion = "1.6.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"
libraryDependencies ++= Seq(
  ("it.unibo.scafi" %% "scafi-core" % scafiVersion).cross(CrossVersion.for3Use2_13),
  ("it.unibo.scafi" %% "scafi-commons" % scafiVersion).cross(CrossVersion.for3Use2_13),
  ("it.unibo.scafi" %% "scafi-simulator" % scafiVersion).cross(CrossVersion.for3Use2_13))
