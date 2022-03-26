import play.api.libs.json.{Json, Reads}

import java.io.{File, BufferedWriter => BW}
import sbt.io.IO

trait Unitdef[U <: UnitInfo]{
  def description: Option[String]
  def si_unit: String
  def dimension: Dimension
  def units: Option[Seq[U]]
  def convertibles: Option[Seq[Convertible]]
  def use: Option[Use]
}

trait UnitInfo{
  def name: String
  def symbol: String
  def aliases: Option[Seq[String]]
  def interval: Option[String]
  def description: Option[String]

  def objectName: String

  def aliasesStr: String = this.aliases match {
    case Some(_aliases) => _aliases.mkString("Seq(\"", "\", \"", "\")")
    case _ => ""
  }
}

case class Dimension(M: Option[Int], L: Option[Int], T: Option[Int], I: Option[Int], Θ: Option[Int], N: Option[Int], J: Option[Int]){
  def getEntries: Map[String, Int] =
    Map("M" -> M, "L" -> L, "T" -> T, "I" -> I, "Θ" -> Θ, "N" -> N, "J" -> J).filter(_._2.nonEmpty).mapValues(_.get)
}

case class Convertible(result: String, from: String, to: String, algorithm: Option[String], factor: Option[String])

case class Use(subpackages: Option[Seq[String]], constants: Option[Boolean])

sealed abstract class UnitType(val name: String)

object UnitType {
  final case object Homogeneous extends UnitType("Homogeneous")
  final case object Linear extends UnitType("Linear")
}

abstract class UnitdefJson(jsonFile: File, val subpackage: String, val unitType: UnitType)
  extends JsonResource(jsonFile){

  val id: String = jsonFile.getName.replace(".json", "")  // Length

  private def getPathSeq: Seq[String] =
    if (subpackage == "") Seq("unit", "defs")
    else Seq("unit", "defs", subpackage)

  protected lazy val packageName: String = GenerationUtil.rootPackage + "." + getPathSeq.mkString(".")

  override protected def getDestFile(destRoot: File): File = {
    val paths = getPathSeq :+ s"$id.scala"
    IO.resolve(destRoot, new File(paths.mkString("/")))
  }
}

abstract class UnitdefJsonAdapter[UD <: Unitdef[U], U <: UnitInfo]
    (jsonFile: File, subpackage: String, unitType: UnitType)
    extends UnitdefJson(jsonFile, subpackage, unitType){

  import GenerationUtil._

  implicit val dimensionReads: Reads[Dimension] = Json.reads[Dimension]
  implicit val convertibleReads: Reads[Convertible] = Json.reads[Convertible]
  implicit val useReads: Reads[Use] = Json.reads[Use]

  def unitdef: UD

  protected def doGenerate(destFile: File): Unit = {
    IO.writer(destFile, "", utf8, append = false) { writer: BW =>
      generateGlobalImports(writer)
      generateQuantity(writer)
      generateUnitTrait(writer)
      generateUnitCompanionObject(writer)
      generateImplementationsOfUnitTrait(writer)
      generateUnitObjects(writer)
      generateUnits(writer)
    }
  }

  private def generateGlobalImports(writer: BW): Unit = {
    writer.write(
      s"""package $packageName
         |
         |import spire.math._
         |""".stripMargin)

    if (needSpireImplicits)
      writer.write("import spire.implicits._\n")

    writer.write(
      s"""
         |import $rootPackage._
         |""".stripMargin)

    this.unitdef.use.foreach{ use =>
      use.subpackages.foreach{ sps =>
        sps.foreach{ sp =>
          if (sp == "") writer.write(s"""import $rootPackage.unit.defs._\n""")
          else              writer.write(s"""import $rootPackage.unit.defs.$sp._\n""")
        }
      }
      use.constants match {
        case Some(true) => writer.write(s"import $rootPackage.Constants\n")
        case _ =>
      }
    }
    writer.write("\n")
  }

  protected def needSpireImplicits: Boolean =
    this.unitdef.units match {
      case Some(_units) => _units.exists(u => u.interval.nonEmpty)
      case  _ => false
    }

  private def generateQuantity(writer: BW): Unit = {
    writer.write(
      s"""class $id[A: Fractional](val value: A, val unit: ${id}Unit)
         |    extends $parentQuantityDeclaration {
         |
         |""".stripMargin)

    this.unitdef.convertibles.foreach{ convs =>
      convs.foreach{ conv =>
        val res = conv.result

        def refineUnit(s: String, defaultType: String): String = regexUnitName.replaceAllIn(s, m => {
          val rType = if (m.group(1) != null) m.group(1) else defaultType
          val rName = m.group(2)
          s"""${rType}UnitObjects.$rName"""
        })

        conv.algorithm match {
          case Some(algo) =>
            require(algo == "reciprocal")
            writer.write(
              s"""
                 |  def to$res: $res[A] =
                 |    new $res(apply(${refineUnit(conv.from, this.id)}).reciprocal, ${refineUnit(conv.to, res)})
                 |
                 |""".stripMargin)
          case _ =>
            val factor = conv.factor match {
              case Some(f) => s""" * implicitly[Fractional[A]].fromReal(${refineFactor(f)})"""
              case _ => ""
            }
            writer.write(
              s"""
                 |  def to$res: $res[A] = new $res(
                 |      apply(${refineUnit(conv.from, this.id)})$factor,
                 |      ${refineUnit(conv.to, res)})
                 |
                 |""".stripMargin)
        }
      }
    }

    generateQuantityOperations(writer)

    writer.write("}\n\n")
  }

  protected def parentQuantityDeclaration: String
  protected def generateQuantityOperations(writer: BW): Unit = ()

  private def generateUnitTrait(writer: BW): Unit = {
    if (this.unitdef.description != null) {
      writer.write(s"/** ${this.unitdef.description} */\n")
    }

    val additionalTraits = getAdditionalTraitsOfUnit.map(" with " + _).mkString("")

    writer.write(
      s"""trait ${id}Unit extends ${unitType.name}Unit[${id}Unit]$additionalTraits{
         |
         |  override def getSIUnit: ${id}Unit = ${id}Unit.getSIUnit
         |  override def dimension: Map[DimensionSymbol, Int] = ${id}Unit.dimension
         |""".stripMargin)

    generateUnitOperations(writer)

    writer.write("}\n\n")
  }

  protected def getAdditionalTraitsOfUnit: Seq[String] = Seq()
  protected def generateUnitOperations(writer: BW): Unit = ()

  private def generateUnitCompanionObject(writer: BW): Unit = {
    writer.write(s"""object ${id}Unit extends UnitInfo[${id}Unit]{\n""")

    //***** Dimension *****
    val entries = this.unitdef.dimension.getEntries
    if (entries.nonEmpty) writer.write("  import DimensionSymbol._\n")

    val dim = entries.map(e => s"""${e._1} -> ${e._2}""").mkString(", ")
    writer.write(
      s"""
         |  val dimension: Map[DimensionSymbol, Int] =
         |    Map[DimensionSymbol, Int]($dim).withDefaultValue(0)
         |""".stripMargin)

    //***** SI Unit *****
    val siUnit = this.unitdef.si_unit
    if (siUnit.contains('*') || siUnit.contains('/')) {
      val regexCompositeUnit(first, op, second) = siUnit

      writer.write(
        s"""
           |  val getSIUnit: ${id}Unit = ${first}Unit.getSIUnit $op ${second}Unit.getSIUnit
           |""".stripMargin)
    } else {
      writer.write(
        s"""
           |  def getSIUnit: ${id}Unit = ${id}UnitObjects.$siUnit
           |""".stripMargin)
    }

    //***** Defined Units *****
    this.unitdef.units match {
      case Some(units) =>
        writer.write(
          s"""
             |  import ${id}UnitObjects._
             |
             |  def getUnits: Seq[${id}Unit] =
             |    ${units.map(_.objectName).mkString("Seq(", ", ", ")")}
             |}
             |""".stripMargin)
      case _ =>
        writer.write(
          s"""
             |  def getUnits: Seq[${id}Unit] = Seq()
             |}
             |""".stripMargin)
    }
  }

  protected def generateImplementationsOfUnitTrait(writer: BW): Unit

  private def generateUnitObjects(writer: BW): Unit = {
    this.unitdef.units.foreach { units =>
      writer.write(
        s"""
           |object ${id}UnitObjects{
           |
           |""".stripMargin)

      units.foreach(generateUnitCaseObject(writer, _))
      writer.write("}\n\n")
    }
  }

  protected def generateUnitCaseObject(writer: BW, u: U): Unit

  protected def generateUnits(writer: BW): Unit
}
