import java.io
import java.io.{BufferedWriter, File}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class UnitCategory(SIUnit: String, products: Array[ProductUnit], quotients: Array[QuotientUnit], units: Array[UnitJson]){
  lazy val _products: Seq[ProductUnit] =
    if (this.products != null) this.products.toList
    else Nil

  lazy val _quotients: Seq[QuotientUnit] =
    if (this.quotients != null) this.quotients.toList
    else Nil
}

case class ProductUnit(firstUnit: String, secondUnit: String)
case class QuotientUnit(numeratorUnit: String, denominatorUnit: String)

// ex)
//{"name":"electronvolt", "symbol":"eV", "interval":"Constants.ElementaryCharge",
//   scalePrefixes":true, "notExact":true},
case class UnitJson(
                     name: String,
                     symbol: String,
                     aliases: Array[String],
                     interval: String,
                     baseUnit: String,
                     scalePrefixes: Boolean,
                     excludePrefixes: Array[String],
                     notExact: Boolean,
                     attributes: Array[Attribute]
                   ){
  def extractInterval: String =
    if(this.interval != null)
      this.interval
    else
      this.attributes.find(_.default) match {
        case Some(a) => a.interval
        case None => throw new RuntimeException(
          "Neither an interval element or a default attribute exists: " + this.name)
      }

  def extractBaseUnit: String =
    if(this.baseUnit != null)
      this.baseUnit
    else if(this.interval != null)
      null
    else
      this.attributes.find(_.default) match {
        case Some(a) => a.baseUnit
        case None => throw new RuntimeException(
          "Neither a baseUnit element or a default attribute exists: " + this.name)
      }

  def _aliases: Seq[String] =
    if (this.aliases != null) this.aliases.toList
    else Nil

  def canonicalizeAndExpandScalePrefixes(): Seq[CanonicalizedUnitJson] = {
    val interval = makeIntervalExpression(extractInterval, extractBaseUnit)
    val aliases: Seq[String] = this._aliases

    if(scalePrefixes){
      val nm = this.symbol +: aliases

      // In this case, always name == objectName
      CanonicalizedUnitJson(this.name, this.name, this.symbol, aliases, interval, this.notExact, Nil) +:
        GenerationUtil.scalePrefixes.map{ p =>
          val al = (p.prefix +: p.aliases).flatMap(ps => nm.map(ns => ps + ns)).tail
          val name = p.name + this.name
          CanonicalizedUnitJson(name, name, p.prefix + this.symbol, nonNull(al),
            s"""$interval * r"${p.scale}"""", this.notExact, Nil)
        }
    }else{
      val atts =
        if(this.attributes == null) Nil
        else this.attributes.toList

      val u = CanonicalizedUnitJson(
        this.name, toObjectName(this.name), this.symbol, aliases, interval, this.notExact, atts)

      val us = atts.map{ a =>
        val name = s"${u.name}(${a.name})"
        CanonicalizedUnitJson(
          name, toObjectName(name), s"${u.symbol}(${a.name})",
          aliases.map(al => s"$al(${a.name})"), makeIntervalExpression(a.interval, a.baseUnit), a.notExact, Nil)
      }

      u +: us
    }
  }

  private def makeIntervalExpression(interval: String, baseUnit: String): String = {
    val i = GenerationUtil.regNum.replaceAllIn(interval, m => s"""r"${interval.substring(m.start, m.end)}"""")

    val bu =
      if(baseUnit == null) ""
      else s""" * ${toObjectName(baseUnit)}.interval"""

    i + bu
  }

  private def toObjectName(s: String): String = {
    val ss = s.replace(' ', '_')
    if (ss.contains("(")) s"""`$ss`"""
    else ss
  }

  private def  nonNull(a: Seq[String]): Seq[String] = if(a == null) Nil else a.toList
}

case class Attribute(name: String, interval: String, baseUnit: String, notExact: Boolean, default: Boolean)

case class CanonicalizedUnitJson(
                                  name: String,
                                  objectName: String,
                                  symbol: String,
                                  aliases: Seq[String],
                                  interval: String,
                                  notExact: Boolean,
                                  attributes: Seq[Attribute])

class LinearUnitResource(jsonFile: File, destDir: File, mainDir: File, val kind: String)
    extends JsonResource(jsonFile, destDir, mainDir){

  val unitCategoryType: Class[_ >: UnitCategory] = new TypeToken[UnitCategory]() {}.getRawType
  val id: String = jsonFile.getName.replace("Units.json", "")  // Length
  val destFilename: String =  id + ".scala"// Length.scala
  val packageName: String = GenerationUtil.rootPackage + ".unit." + kind
  val unitCategory: UnitCategory =
    IO.reader(jsonFile, UTF8) { reader =>
      gson.fromJson(reader, unitCategoryType).asInstanceOf[UnitCategory]
    }

  override def isLinearUnit: Boolean = true

  protected def doGenerate(jsons: Seq[JsonResource]): Unit = {
    IO.writer(this.destFile, "", UTF8, append = false) { writer: io.BufferedWriter =>
      val units = this.unitCategory.units.flatMap(_.canonicalizeAndExpandScalePrefixes())

      writer.write(
        s"""package $packageName
           |
           |import spire.math.Real
           |import spire.math.Fractional
           |import spire.implicits._
           |import ${GenerationUtil.rootPackage}._
           |
           |class $id[A: Fractional](val value: A, val unit: ${id}Unit)
           |    extends LinearQuantity[$id[A], A, ${id}Unit] {
           |
           |  override protected def newQuantity(value: A, unit: ${id}Unit): $id[A] = new $id(value, unit)
           |}
           |
           |trait ${id}Unit extends LinearUnit[${id}Unit]{
           |  override def getSIUnit: ${id}Unit = ${id}UnitObjects.getSIUnit
           |
           |""".stripMargin)

      val linearUnits = jsons.filter(_.isLinearUnit).map(_.asInstanceOf[LinearUnitResource])

      val mul: Seq[(BriefType, BriefType)] =
        linearUnits.flatMap(lu => lu.unitCategory._products.flatMap{
          case p if p.firstUnit == this.id =>
            Seq((BriefType(p.secondUnit, linearUnits), BriefType(lu.kind, lu.id)))
          case _ => Nil
        })

      val div: Seq[(BriefType, BriefType)] =
        linearUnits.flatMap(lu => lu.unitCategory._quotients.flatMap{
          case p if p.numeratorUnit == id =>
            Seq((BriefType(p.denominatorUnit, linearUnits), BriefType(lu.kind, lu.id)))
          case _ => Nil
        })

      mul.foreach{ p =>
        Seq(p._1, p._2).foreach{ bt =>
          if (bt.kind != "" && bt.kind != this.kind)
            writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.${bt.kind}.${bt.id}Unit\n""")
        }

        val secondType = p._1.id
        val resultType = p._2.id
        val secondId = headToLower(secondType)
        writer.write(
          s"""
             |  def *(${secondId}Unit: ${secondType}Unit): ${resultType}Unit =
             |    new ProductUnit[${resultType}Unit, ${id}Unit, ${secondType}Unit](${id}Unit.this, ${secondId}Unit) with ${resultType}Unit
             |
             |""".stripMargin)
      }

      div.foreach{ p =>
        Seq(p._1, p._2).foreach{ bt =>
          if (bt.kind != "" && bt.kind != this.kind)
            writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.${bt.kind}.${bt.id}Unit\n""")
        }

        val denoType = p._1.id
        val resultType = p._2.id
        val denoId = headToLower(denoType)
        writer.write(
          s"""
             |  def /(${denoId}Unit: ${denoType}Unit): ${resultType}Unit =
             |    new QuotientUnit[${resultType}Unit, ${id}Unit, ${denoType}Unit](${id}Unit.this, ${denoId}Unit) with ${resultType}Unit
             |
             |""".stripMargin)
      }

      writer.write(
        s"""}
           |
           |class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
           |  extends ${id}Unit
           |
           |""".stripMargin)

      generateAttributesCode(writer, id, units)
      writer.write("\n")
      generateUnitObjectCode(writer, id, this.unitCategory.SIUnit, units, linearUnits)
      writer.write("\n")
      generateUnitsCode(writer, id, units)
    }
  }

  case class BriefType(kind: String, id: String)

  object BriefType{
    def apply(id: String, linearUnits: Seq[LinearUnitResource]): BriefType =
      linearUnits.find(_.id == id) match {
        case Some(lu) => BriefType(lu.kind, id)
        case _ => throw new RuntimeException(s"""Unknown unit appears: $id""")
      }
  }

  private def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)

  private def generateAttributesCode(writer: BufferedWriter, id: String, units: Seq[CanonicalizedUnitJson]): Boolean = {
    val attUnits = units.filter(_.attributes.nonEmpty)
    if (attUnits.isEmpty) return false

    attUnits.foreach { u =>
      writer.write(s"""sealed trait ${u.objectName}Attribute\n""")
    }

    // [gregorian: Seq(month, year, decade, ...), julian: Seq(year, decade, ...), ...]
    val map = attUnits.flatMap(u => u.attributes.map(a => (u.objectName, a.name)))
      .groupBy(_._2)
      .mapValues(v => v.map(_._1))
    writer.write(s"\nobject ${id}Attributes{\n")
    map.foreach { u =>
      writer.write(s"""  final object ${u._1} extends ${u._2.map(_ + "Attribute").mkString(" with ")}\n""")
    }
    writer.write("}\n")
    true
  }

  private def generateUnitObjectCode(
                                      writer: BufferedWriter, id: String, siUnit: String, units: Seq[CanonicalizedUnitJson],
                                      linearUnits: Seq[LinearUnitResource]): Unit = {

    writer.write(s"object ${id}UnitObjects{\n")

    if(units.exists(u => u.interval.contains("Constants")))
      writer.write(s"  import ${GenerationUtil.rootPackage}.unit.Constants\n\n")

    units.foreach { u =>
      val aliases =
        if (u.aliases.isEmpty) "Nil"
        else u.aliases.mkString("Seq(\"", "\", \"", "\")")

      val notExact =
        if (!u.notExact) ""
        else " with NotExact"

      // final case object metre extends SimpleLengthUnit("metre", "m", Nil, r"1")
      writer.write(
        s"""  final object ${u.objectName} extends Default${id}Unit("${u.name}", "${u.symbol}", $aliases, ${u.interval})$notExact\n""")
    }

    writer.write("\n")

    //***** SI Unit *****
    if (siUnit.contains('*') || siUnit.contains('/')){
      val GenerationUtil.regCompositeUnit(first, op, second) = siUnit

      List(first, second).map(BriefType(_, linearUnits)).filterNot(_.kind == this.kind).foreach{ bt =>
        writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.${bt.kind}.${bt.id}UnitObjects\n""")
      }

      writer.write(
        s"""
           |  val getSIUnit: ${id}Unit = ${first}UnitObjects.getSIUnit $op ${second}UnitObjects.getSIUnit
           |""".stripMargin)
    }else{
      writer.write(
        s"""
           |  def getSIUnit: ${id}Unit = $siUnit
           |""".stripMargin)
    }

    writer.write(
      s"""
         |  def getUnits: Seq[${id}Unit] =
         |    ${units.map(_.objectName).mkString("Seq(", ", ", ")")}
         |}
         |""".stripMargin)
  }

  private def generateUnitsCode(
                                 writer: BufferedWriter, id: String, units: Seq[CanonicalizedUnitJson]): Unit = {
    writer.write(s"object ${id}Units{\n")

    units.filterNot(_.name.contains("(")).foreach { u =>

      // def m: LengthUnit = LengthUnitObjects.metre
      writer.write(s"""  def ${u.symbol}: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")

      // def xu(a: xunitAttribute): LengthUnit = a match {
      //   case CuKα1 => LengthUnitObjects.`xunit(CuKα1)`
      //   case MoKα1 => LengthUnitObjects.`xunit(MoKα1)`
      // }
      if (u.attributes.nonEmpty) {
        writer.write(s"""  def ${u.symbol}(a: ${u.objectName}Attribute): ${id}Unit = a match { \n""")
        u.attributes.foreach { a =>
          writer.write(s"""    case ${id}Attributes.${a.name} => ${id}UnitObjects.`${u.objectName}(${a.name})`\n""")
        }
        writer.write("  }\n")
      }

      u.aliases.foreach { al =>
        writer.write(s"""  def $al: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")

        // def nmi(a: nautical_mileAttribute): LengthUnit = NM(a)
        if (u.attributes.nonEmpty && !al.contains("_")) {
          writer.write(s"""  def $al(a: ${u.objectName}Attribute): ${id}Unit = ${u.symbol}(a)\n""")
        }
      }
    }

    writer.write(
      s"""
         |  def getSIUnit: ${id}Unit = ${id}UnitObjects.getSIUnit
         |  def getUnits: Seq[${id}Unit] = ${id}UnitObjects.getUnits
         |}
         |""".stripMargin)
  }
}