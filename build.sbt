name := "multiverse"

version := "0.1"

organization := "org.waman"

scalaVersion := "2.11.7"

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")

val encoding = settingKey[String]("source encoding")

javaVersion := "1.8"

encoding := "UTF-8"

libraryDependencies ++= Seq(
  "org.spire-math" % "spire_2.11" % "0.8.2",
  "org.waman" % "scalatest-util" % "0.1" % "test"
)

//***** Options & Dependencies *****
javacOptions ++= Seq(
  "-source", javaVersion.value,
  "-target", javaVersion.value,
  "-encoding", encoding.value
)

scalacOptions ++= Seq(
  "-Xlint",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-encoding", encoding.value
)

//***** Running *****
fork := true

initialCommands in console := "import org.waman.multiverse.MKSUnitSystem._"

crossPaths := false
