// Turn this project into a Scala.js project by importing these settings
scalaJSSettings

name := "Example"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

ScalaJSKeys.persistLauncher := true

ScalaJSKeys.persistLauncher in Test := false

libraryDependencies ++= Seq(
    "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6",
    "org.scala-lang.modules.scalajs" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test",
    "com.scalatags" %%% "scalatags" % "0.3.8",
    "org.scala-lang.modules.scalajs" %%% "scalajs-jquery" % "0.6",
    "com.scalarx" %%% "scalarx" % "0.2.5"
)
