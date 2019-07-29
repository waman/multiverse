name := "multiverse"

version := "0.3"

organization := "org.waman"

scalaVersion := "2.12.8"

//***** Custom settings *****
val javaVersion = settingKey[String]("javac source/target version")

val encoding = settingKey[String]("source encoding")

javaVersion := "10"

encoding := "UTF-8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.16.2",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
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

//***** Source Generation *****
sourceManaged in Compile := file((sourceDirectory in Compile).value.getAbsolutePath + "/src_managed")
//sourceManaged in Compile := file("src/main/src_managed")

sourceGenerators in Compile += Def.task {
  val info = (resourceDirectory in Compile).value / "physical-units"
  val destDir = (sourceManaged in Compile).value
  MultiverseSourceGenerator.generate(info, destDir)
}.taskValue

cleanFiles += (sourceManaged in Compile).value

//initialCommands in console :=
//  """import org.waman.multiverse._
//    |import org.waman.multiverse.unitsystem.MKSUnitSystem._
//    |import scala.language.postfixOps
//  """.stripMargin

crossPaths := false
