name := "multiverse"

version := "0.2"

organization := "org.waman"

scalaVersion := "2.12.8"

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")

val encoding = settingKey[String]("source encoding")

javaVersion := "10"

encoding := "UTF-8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.16.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
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

//initialCommands in console :=
//  """import org.waman.multiverse._
//    |import org.waman.multiverse.unitsystem.MKSUnitSystem._
//    |import scala.language.postfixOps
//  """.stripMargin

crossPaths := false
