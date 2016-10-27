name := "ResistorChallenge"

version := "1.0"

scalaVersion := "2.11.8"

mainClass := Some("Main")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.5",
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
)
    