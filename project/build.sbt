scalaVersion := "2.11.7"

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")

val encoding = settingKey[String]("source encoding")

javaVersion := "1.8"

encoding := "UTF-8"

libraryDependencies ++= Seq(
//  "org.spire-math" % "spire_2.11" % "0.8.2",
  "org.waman" % "gluino" % "0.2",
  "com.google.code.gson" % "gson" % "2.6.2"
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