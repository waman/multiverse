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
  "org.spire-math" % "spire_2.11" % "0.11.0",
  "org.waman" % "scalatest-util" % "0.3" % "test",
  "org.scalacheck" % "scalacheck_2.11" % "1.12.5" % "test",
  "org.waman" % "gluino" % "0.2" % "test",
  "com.google.code.gson" % "gson" % "2.6.2" % "test"
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

//***** Source Generation *****
sourceManaged in Compile := file((sourceDirectory in Compile).value.getAbsolutePath + "/src_managed")
//sourceManaged in Compile := file("src/main/src_managed")

sourceGenerators in Compile += Def.task {
  val rsrc = (resourceDirectory in Compile).value.toPath
  val destDir = (sourceManaged in Compile).value.toPath
  MultiverseSourceGenerator.generate(rsrc, destDir).map(_.toFile)
}.taskValue

cleanFiles += (sourceManaged in Compile).value

//***** Running *****
fork := true

initialCommands in console :=
  """import org.waman.multiverse._
    |import org.waman.multiverse.MKSUnitSystem._
    |import scala.language.postfixOps
  """.stripMargin

crossPaths := false
