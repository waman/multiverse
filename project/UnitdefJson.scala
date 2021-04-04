import java.io.{File, BufferedWriter => BW}

import sbt.io.IO

trait UnitCategory[U <: UnitInfo]{
  def description: String
  def SIUnit: String
  def dimension: Dimension
  def units: Array[U]
  def convertibles: Array[Convertible]
  def use: Use

  import GenerationUtil._

  lazy val _units: Seq[U] = toSeq(this.units)
  def _convertibles: Seq[Convertible] = toSeq(this.convertibles)
}

trait UnitInfo{
  def name: String
  def symbol: String
  def aliases: Array[String]
  def interval: String
  def description: String

  import GenerationUtil._

  def objectName: String
  def _aliases: Seq[String] = toSeq(this.aliases)

  def aliasesStr: String = this._aliases.mkString("Seq(\"", "\", \"", "\")")
}

case class Dimension(M: Int, L: Int, T: Int, I: Int, Θ: Int, N: Int, J: Int){
  def getEntries: Map[String, Int] =
    Map("M" -> M, "L" -> L, "T" -> T, "I" -> I, "Θ" -> Θ, "N" -> N, "J" -> J).filter(_._2 != 0)
}

case class Convertible(result: String, from: String, to: String, algorithm: String, factor: String)

case class Use(subpackages: Array[String], selfUnits: Boolean, constants: Boolean){
  def _subpackages: Seq[String] = GenerationUtil.toSeq(this.subpackages)
}

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

abstract class UnitdefJsonAdapter[UC <: UnitCategory[U], U <: UnitInfo]
    (jsonFile: File, subpackage: String, unitType: UnitType)
    extends UnitdefJson(jsonFile, subpackage, unitType){

  import GenerationUtil._

  def unitCategory: UC

  protected def doGenerate(destFile: File): Unit = {
    IO.writer(destFile, "", utf8, append = false) { writer: BW =>
      generateGlobalImports(writer)
      generateQuantity(writer)
      generateUnitTrait(writer)
      generateUnitCompanionObject(writer)
      generateImplsOfUnitTrait(writer)
      generateUnitObjects(writer)
      generateUnits(writer)
    }
  }

  private def generateGlobalImports(writer: BW): Unit = {
    writer.write(
      s"""package $packageName
         |
         |import spire.math._
         |import spire.implicits._
         |
         |import $rootPackage._
         |
         |""".stripMargin)

    if (this.unitCategory.use == null) return
    val use = this.unitCategory.use
    use._subpackages.foreach{ sp =>
      if (sp == "")
        writer.write(s"""import $rootPackage.unit.defs._\n""")
      else
        writer.write(s"""import $rootPackage.unit.defs.$sp._\n""")
    }
    if (use.constants) {
      writer.write(s"import $rootPackage.Constants\n")
    }

    writer.write("\n")
  }

  private def generateQuantity(writer: BW): Unit = {
    writer.write(
      s"""class $id[A: Fractional](val value: A, val unit: ${id}Unit)
         |    extends $parentQuantityDeclaration {
         |
         |""".stripMargin)

    this.unitCategory._convertibles.foreach{ conv =>
      val res = conv.result

      def refineUnit(s: String, defaultType: String): String = regexUnitName.replaceAllIn(s, m => {
        val rType = if (m.group(1) != null) m.group(1) else defaultType
        val rName = m.group(2)
        s"""${rType}UnitObjects.$rName"""
      })

      if (conv.algorithm != null){
        require(conv.algorithm == "reciprocal")
        writer.write(
          s"""
             |  def to$res: $res[A] =
             |    new $res(apply(${refineUnit(conv.from, this.id)}).reciprocal, ${refineUnit(conv.to, res)})
             |
             |""".stripMargin)
      } else {
        val factor = if (conv.factor != null) s""" * implicitly[Fractional[A]].fromReal(${conv.factor})""" else ""
        writer.write(
          s"""
             |  def to$res: $res[A] = new $res(
             |      apply(${refineUnit(conv.from, this.id)})$factor,
             |      ${refineUnit(conv.to, res)})
             |
             |""".stripMargin)
      }
    }

    generateQuantityOperations(writer)

    writer.write("}\n\n")
  }

  protected def parentQuantityDeclaration: String
  protected def generateQuantityOperations(writer: BW): Unit = ()

  private def generateUnitTrait(writer: BW): Unit = {
    if (this.unitCategory.description != null) {
      writer.write(s"/** ${this.unitCategory.description} */\n")
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
    val entries = this.unitCategory.dimension.getEntries
    if (entries.nonEmpty) writer.write("  import DimensionSymbol._\n")

    val dim = entries.map(e => s"""${e._1} -> ${e._2}""").mkString(", ")
    writer.write(
      s"""
         |  val dimension: Map[DimensionSymbol, Int] =
         |    Map[DimensionSymbol, Int]($dim).withDefaultValue(0)
         |""".stripMargin)

    //***** SI Unit *****
    val siUnit = this.unitCategory.SIUnit
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
    if (this.unitCategory._units.nonEmpty) writer.write(s"""  import ${id}UnitObjects._\n""")
    writer.write(
      s"""
         |  def getUnits: Seq[${id}Unit] =
         |    ${this.unitCategory._units.map(_.objectName).mkString("Seq(", ", ", ")")}
         |}
         |""".stripMargin)
  }

  protected def generateImplsOfUnitTrait(writer: BW): Unit

  private def generateUnitObjects(writer: BW): Unit = {
    writer.write(
      s"""
          |object ${id}UnitObjects{
          |  import spire.implicits._
          |
          |""".stripMargin)

    val units = this.unitCategory._units

//    if (units.filter(_.interval != null).map(_.interval).exists(s => s == "1" || s.contains("r\""))) {
//      writer.write("  import spire.implicits._\n\n")
//    }

    //***** Unit Objects *****
    units.foreach(generateUnitCaseObject(writer, _))

    writer.write("}\n\n")
  }

  protected def generateUnitCaseObject(writer: BW, u: U): Unit

  protected def generateUnits(writer: BW): Unit
}
