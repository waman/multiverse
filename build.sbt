//lazy val scala213 = "2.13.5"
lazy val scala212 = "2.12.13"
//lazy val scala211 = "2.11.12"
//lazy val supportedScalaVersions = List(scala213, scala212, scala211)

// ThisBuild / name := "multiverse"
ThisBuild / version := "0.15"
ThisBuild / organization := "org.waman"
ThisBuild / scalaVersion := scala212

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")
val encoding = settingKey[String]("source encoding")

ThisBuild / javaVersion := "11"
ThisBuild / encoding := "UTF-8"

lazy val root = (project in file(".")).withId("multiverse")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.17.0-RC1",
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test
    ),
    javacOptions ++= Seq(
      "-source", javaVersion.value,
      "-target", javaVersion.value,
      "-encoding", encoding.value
    ),
    scalacOptions ++= Seq(
      "-Xlint",
      "-deprecation",
      "-unchecked",
      "-feature",
      "-encoding", encoding.value
    ),
//    crossScalaVersions := Nil,
    crossPaths := false
  )

//***** Source Generation *****
Compile / sourceManaged := file((Compile / sourceDirectory).value.getAbsolutePath + "/src_managed")

Compile / sourceGenerators += Def.task {
  val info = (Compile / resourceDirectory).value / "unitdefs/json_simplified"
  val srcManaged = (Compile / sourceManaged).value
  MultiverseSourceGenerator.generate(info, srcManaged)
}.taskValue

cleanFiles += (Compile / sourceManaged).value

//***** Publish *****
githubOwner := "waman"
githubRepository := "multiverse"

//***** Running *****
fork := true

//initialCommands in console :=
//  """import multiverse.implicits._
//    |import multiverse.unit.BasicUnits._
//  """.stripMargin