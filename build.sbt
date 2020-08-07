//lazy val scala213 = "2.13.1"
lazy val scala212 = "2.12.12"
//lazy val scala211 = "2.11.12"
//lazy val supportedScalaVersions = List(scala213, scala212, scala211)

ThisBuild / name := "multiverse"
ThisBuild / version := "0.8"
ThisBuild / organization := "org.waman"
ThisBuild / scalaVersion := scala212

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")
val encoding = settingKey[String]("source encoding")

ThisBuild / javaVersion := "11"
ThisBuild / encoding := "UTF-8"

lazy val root = (project in file("."))
    .settings(
      name := "multiverse",
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
      crossScalaVersions := Nil,
      publish / skip := true
    )

//***** Source Generation *****
Compile / sourceManaged := file((Compile / sourceDirectory).value.getAbsolutePath + "/src_managed")

Compile / sourceGenerators += Def.task {
  val info = (Compile / resourceDirectory).value / "unitdefs"
  val srcManaged = (Compile / sourceManaged).value
  MultiverseSourceGenerator.generate(info, srcManaged)
}.taskValue

cleanFiles += (Compile / sourceManaged).value

//***** Running *****
fork := true

//initialCommands in console :=
//  """import multiverse.implicits._
//    |import multiverse.unit.BasicUnits._
//  """.stripMargin

crossPaths := false