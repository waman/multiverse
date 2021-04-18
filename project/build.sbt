scalaVersion := "2.12.13"

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")
val encoding = settingKey[String]("source encoding")

ThisBuild / javaVersion := "11"
ThisBuild / encoding := "UTF-8"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.9.2"
)

//***** Options & Dependencies *****
javacOptions ++= Seq(
  "-source", javaVersion.value,
  "-target", javaVersion.value,
  "-encoding", encoding.value
)

scalacOptions ++= Seq(
//  "-Xlint",
//  "-deprecation",
//  "-unchecked",
//  "-feature",
  "-encoding", encoding.value
)