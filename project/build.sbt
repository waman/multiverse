scalaVersion := "2.12.12"

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")

val encoding = settingKey[String]("source encoding")

javaVersion := "11"

encoding := "UTF-8"

libraryDependencies ++= Seq(
  "com.google.code.gson" % "gson" % "2.8.5"
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