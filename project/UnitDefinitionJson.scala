import java.io.{File, BufferedWriter => BW}

import sbt.io.IO

trait RawUnitInfo[U <: UnitInfo]{
  def symbol: String
  def aliases: Array[String]
  def excludePrefixes: Array[String]

  lazy val symbols: Seq[String] = this.symbol +: this._aliases
  lazy val _aliases: Seq[String] = GenerationUtil.toSeq(this.aliases)
  lazy val _excludePrefixes: Seq[String] = GenerationUtil.toSeq(this.excludePrefixes)

  def expandScalePrefixesAndAttributes(jsons: JsonResources): Seq[U]
}

trait UnitInfo{
  def name: String
  def objectName: String
  def symbol: String
  def aliases: Seq[String]
  def interval: String
  def baseUnit: String
  def attributes: Seq[Attribute]
}

trait UnitCategory[RU <: RawUnitInfo[U], U <: UnitInfo]{
  def SIUnit: String
  def dimension: Dimension
  def units: Array[RU]
  def convertibles: Array[Convertible]

  lazy val _units: Seq[RU] = GenerationUtil.toSeq(this.units)
  lazy val _convertibles: Seq[Convertible] = GenerationUtil.toSeq(this.convertibles)
}

case class Dimension(M: Int, L: Int, T: Int, I: Int, Θ: Int, N: Int, J: Int){
  def getEntries: Map[String, Int] =
    Map("M" -> M, "L" -> L, "T" -> T, "I" -> I, "Θ" -> Θ, "N" -> N, "J" -> J).filter(_._2 != 0)
}

case class Convertible(target: String, from: String, to: String, algorithm: String, factor: String){
  require(algorithm != null || factor != null)
  require(algorithm == null || factor == null)
}

abstract class UnitDefinitionJson(val unitType: String, jsonFile: File, destDir: File, val subpackage: String)
  extends SourceGeneratorJson(jsonFile, destDir){

  val id: String = jsonFile.getName.replace("Units.json", "")  // Length
  val destFilename: String =  id + ".scala"// Length.scala
  val packageName: String = GenerationUtil.rootPackage + ".unit." + subpackage
}

abstract class UnitDefinitionJsonAdapter[UC <: UnitCategory[RU, U], RU <: RawUnitInfo[U], U <: UnitInfo, OP]
    (unitType: String, jsonFile: File, destDir: File, subpackage: String)
    extends UnitDefinitionJson(unitType, jsonFile, destDir, subpackage){

  import GenerationUtil._

  def unitCategory: UC

  protected def generateImportsOfExtraUnitTypes(writer: BW, jsons: JsonResources,
                                                types: Seq[String], indentCount: Int, imported: String*): Unit = {
    val indent = " " * indentCount
    types.distinct.map(jsons.searchUnitDefinition).foreach{ ud =>
      imported.filterNot(!_.contains("._") && ud.subpackage == this.subpackage)
        .map(s => s"""${indent}import $rootPackage.unit.${ud.subpackage}.${ud.id}$s\n""").foreach(writer.write)
    }
  }

  protected def doGenerate(jsons: JsonResources): Unit = {
    IO.writer(this.destFile, "", utf8, append = false) { writer: BW =>

      val units = getUnits(jsons)
      val op = createOptions(jsons)

      generateGlobalImports(writer, jsons, op)
      generateQuantity(writer, jsons, op)
      generateUnitTrait(writer, jsons, op)
      generateUnitCompanionObject(writer, jsons, units)
      generateImplsOfUnitTrait(writer)
      generateAttributes(writer, jsons, units)
      generateUnitObjects(writer, jsons, units)
      generateUnits(writer, jsons, units)
    }
  }

  protected def getUnits(jsons: JsonResources): Seq[U]
  protected def createOptions(jsons: JsonResources): OP


  private def generateGlobalImports(writer: BW, jsons: JsonResources, op: OP): Unit = {
    writer.write(
      s"""package $packageName
         |
         |import spire.math.Real
         |import spire.math.Fractional
         |""".stripMargin)

    this.id match {
      case "TimeSquared" | "VolumeFlow" | "Torque" | "EquivalentDoseRate" =>
      case _ => writer.write("import spire.implicits._\n")
    }

    writer.write(
      s"""import $rootPackage._
         |
         |""".stripMargin)

    generateExtraGlobalImports(writer, jsons, op)

    writer.write("\n")
  }

  protected def generateExtraGlobalImports(writer: BW, jsons: JsonResources, op: OP): Unit = ()

  private def generateQuantity(writer: BW, jsons: JsonResources, op: OP): Unit = {
    writer.write(
      s"""class $id[A: Fractional](val value: A, val unit: ${id}Unit)
         |    extends $parentQuantityDecl {
         |
         |""".stripMargin)

    generateQuantityExtraContents(writer, jsons, op)

    if (this.unitCategory._convertibles.filterNot(_.factor == null).exists(_.factor.contains("Constants")))
      writer.write(s"""  import $rootPackage.unit.Constants\n""")

    this.unitCategory._convertibles.foreach{ conv =>
      generateImportsOfExtraUnitTypes(writer, jsons, Seq(conv.target), 2, "")

      if (!conv.to.contains('.'))
        generateImportsOfExtraUnitTypes(writer, jsons, Seq(conv.target), 2, "UnitObjects")

      val extraTypes = Seq(conv.from, conv.to).flatMap(extractUnitTypes)
      generateImportsOfExtraUnitTypes(writer, jsons, extraTypes, 2, "UnitObjects")

      val from =
        if (conv.from.contains('.')) refineUnitNamesInConvertible(conv.from)
        else s"""${id}UnitObjects.${conv.from}"""

      val to =
        if (conv.to.contains('.')) refineUnitNamesInConvertible(conv.to)
        else s"""${conv.target}UnitObjects.${conv.to}"""

      if (conv.algorithm != null && conv.algorithm == "reciprocal"){
        writer.write(
          s"""
             |  def to${conv.target}: ${conv.target}[A] =
             |    new ${conv.target}(apply($from).reciprocal, $to)
             |
             |""".stripMargin)

      } else {
        val factor =
          if (conv.factor == "1") ""  // for Temperature <-> AbsoluteTemperature
          else s""" * implicitly[Fractional[A]].fromReal(${refineNumbers(conv.factor)})"""

        writer.write(
          s"""
             |  def to${conv.target}: ${conv.target}[A] = new ${conv.target}(
             |      apply($from)$factor,
             |      $to)
             |
             |""".stripMargin)
      }
    }
    writer.write("}\n\n")
  }

  protected def parentQuantityDecl: String
  protected def generateQuantityExtraContents(writer: BW, jsons: JsonResources, op: OP): Unit = ()

  private def generateUnitTrait(writer: BW, jsons: JsonResources, op: OP): Unit = {
    writer.write(
      s"""trait ${id}Unit extends ${unitType}Unit[${id}Unit]{
         |
         |  override def getSIUnit: ${id}Unit = ${id}Unit.getSIUnit
         |  override def dimension: Map[DimensionSymbol, Int] = ${id}Unit.dimension
         |
         |""".stripMargin)

    generateUnitTraitExtraContents(writer, jsons,op)

    writer.write("}\n\n")
  }

  protected def generateUnitTraitExtraContents(writer: BW, jsons: JsonResources, op: OP): Unit = ()

  protected def generateImplsOfUnitTrait(writer: BW): Unit

  private def generateUnitCompanionObject(writer: BW, jsons: JsonResources, units: Seq[U]): Unit = {
    writer.write(s"""object ${id}Unit extends UnitInfo[${id}Unit]{\n""")

    //***** Dimension *****
    val entries = this.unitCategory.dimension.getEntries
    if (entries.nonEmpty) writer.write("  import DimensionSymbol._\n")

    val dim = entries.map(e => s"""${e._1} -> ${e._2}""").mkString(", ")
    writer.write(
      s"""  val dimension: Map[DimensionSymbol, Int] =
         |    Map[DimensionSymbol, Int]($dim).withDefaultValue(0)
         |
         |""".stripMargin)

    //***** SI Unit *****
    val siUnit = this.unitCategory.SIUnit
    if (siUnit.contains('*') || siUnit.contains('/')) {
      val regexCompositeUnit(first, op, second) = siUnit

      generateImportsOfExtraUnitTypes(writer, jsons, List(first, second), 2, "Unit")

      writer.write(
        s"""  val getSIUnit: ${id}Unit = ${first}Unit.getSIUnit $op ${second}Unit.getSIUnit
           |
           |""".stripMargin)
    } else {
      writer.write(
        s"""  def getSIUnit: ${id}Unit = ${id}UnitObjects.$siUnit
           |
           |""".stripMargin)
    }

    //***** Defined Units *****
    if (units.nonEmpty) writer.write(s"""  import ${id}UnitObjects._\n""")
    writer.write(
      s"""  def getUnits: Seq[${id}Unit] =
         |    ${units.map(_.objectName).mkString("Seq(", ", ", ")")}
         |}
         |
         |""".stripMargin)
  }

  protected def generateAttributes(writer: BW, jsons: JsonResources, units: Seq[U]): Unit = ()

  protected def attributeId: String = this.id

  private def generateUnitObjects(writer: BW, jsons: JsonResources, units: Seq[U]): Unit = {
    writer.write(s"object ${id}UnitObjects{\n")

    if (units.filter(_.interval != null).exists(_.interval.contains("Constants")))
      writer.write(s"""  import $rootPackage.unit.Constants\n""")

    val types = units.filter(_.baseUnit != null).map(_.baseUnit).flatMap(extractUnitTypes)
    generateImportsOfExtraUnitTypes(writer, jsons, types, 2, "UnitObjects")

    //    if (id == "TimeSquared")
    //      writer.write(s"""  import $rootPackage.unit.basic.TimeUnitObjects\n""")

    writer.write("\n")

    //***** Unit Objects *****
    units.foreach(generateUnitCaseObject(writer, _))

    writer.write("}\n\n")
  }

  protected def generateUnitCaseObject(writer: BW, unit: U): Unit

  protected def makeIntervalExpression(interval: String, baseUnit: String): String =
    if (baseUnit == null) {
      if (interval == null) "1" else refineNumbers(interval)

    } else {
      val baseUnitInterval = refineUnitNamesInBaseUnit(baseUnit)
      if (interval == null) baseUnitInterval
      else s"""${refineNumbers(interval)} * $baseUnitInterval"""
    }

  private def generateUnits(writer: BW, jsons: JsonResources, units: Seq[U]): Unit = {
    writer.write(s"""object ${id}Units{\n""")

    units.filterNot(_.name.contains("(")).foreach { u =>
      val sym = escapeSymbol(u.symbol)

      // def m: LengthUnit = LengthUnitObjects.metre
      writer.write(s"""  def $sym: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")

      // def xu(a: xunitAttribute): LengthUnit = a match {
      //   case MetricAttributes.CuKα1 => LengthUnitObjects.`xunit(CuKα1)`
      //   case MetricAttributes.MoKα1 => LengthUnitObjects.`xunit(MoKα1)`
      // }
      if (u.attributes.nonEmpty) {
        writer.write(s"""  def $sym(a: ${u.objectName}Attribute): ${id}Unit = a match { \n""")
        u.attributes.foreach { a =>
          writer.write(s"""    case ${attributeId}Attributes.${a.name} => ${id}UnitObjects.`${u.objectName}(${a.name})`\n""")
        }
        writer.write("  }\n")
      }

      u.aliases.foreach { al =>
        val als = escapeSymbol(al)
        writer.write(s"""  def $als: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")

        // def nmi(a: nautical_mileAttribute): LengthUnit = NM(a)
        if (u.attributes.nonEmpty && !als.contains("_")) {  // the second condition is for removing ones like cal_IT(IT)
          writer.write(s"""  def $als(a: ${u.objectName}Attribute): ${id}Unit = $sym(a)\n\n""")
        }
      }
    }

    writer.write("}")
  }
}
