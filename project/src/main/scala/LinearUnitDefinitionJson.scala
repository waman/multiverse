import java.io
import java.io.{BufferedWriter, File}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class LinearUnitCategory(SIUnit: String, composites: Array[String], units: Array[LinearUnit]){
  def _units: Seq[LinearUnit] = if (this.units != null) this.units else Nil
}

// ex)
//{"name":"electronvolt", "symbol":"eV", "interval":"Constants.ElementaryCharge",
//   scalePrefixes":true, "notExact":true},
case class LinearUnit(
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
  import GenerationUtil.toObjectName

  def _aliases: Seq[String] =
    if (this.aliases != null) this.aliases else Nil

  lazy val _excludePrefixes: Seq[String] =
    if (this.excludePrefixes != null) this.excludePrefixes else Nil

  def extractInterval: String =
    if(this.interval != null) {
      this.interval
    } else {
      this.attributes.find(_.default) match {
        case Some(a) => a.interval
        case None => throw new RuntimeException(
          "Neither an interval element or a default attribute exists: " + this.name)
      }
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

  def canonicalizeAndExpandScalePrefixes(prefixes: Seq[ScalePrefix]): Seq[CanonicalizedLinearUnit] = {
    if (this.interval == null && this.baseUnit == "SIUnit") {
      // For Area, Volume, and TimeSquared units
      return Seq(CanonicalizedLinearUnit(
        this.name, toObjectName(this.name), this.symbol, this._aliases,
        "SIUnit", this.notExact, Nil, referToSIUnit=true))
    }

    val interval = makeIntervalExpression(extractInterval, extractBaseUnit)
    val aliases = this._aliases

    if(scalePrefixes){
      val nm = this.symbol +: aliases

      // In this case, always name == objectName
      CanonicalizedLinearUnit(this.name, this.name, this.symbol, aliases, interval, this.notExact, Nil, referToSIUnit=false) +:
        prefixes.filterNot(p => this._excludePrefixes.contains(p.prefix)).map{ p =>
          val al = (p.prefix +: p._aliases).flatMap(ps => nm.map(ns => ps + ns)).tail
          val name = p.name + this.name
          CanonicalizedLinearUnit(name, name, p.prefix + this.symbol, nonNull(al),
            s"""$interval * r"${p.scale}"""", this.notExact, Nil, referToSIUnit=false)
        }
    }else{
      val atts =
        if(this.attributes == null) Nil
        else this.attributes.toList

      val u = CanonicalizedLinearUnit(
        this.name, toObjectName(this.name), this.symbol, aliases, interval, this.notExact, atts, referToSIUnit=false)

      val us = atts.map{ a =>
        val name = s"${u.name}(${a.name})"
        CanonicalizedLinearUnit(
          name, toObjectName(name), s"${u.symbol}(${a.name})", aliases.map(al => s"$al(${a.name})"),
          makeIntervalExpression(a.interval, a.baseUnit), a.notExact, Nil, referToSIUnit=false)
      }

      u +: us
    }
  }

  private def makeIntervalExpression(interval: String, baseUnit: String): String = {
    val interval1 = GenerationUtil.refineNumber(interval)
    val interval2 = interval1.replaceAll("Pi", "Constants.Pi")
    val interval3 =
      if(baseUnit == null) interval2
      else s"""$interval2 * ${GenerationUtil.toObjectName(baseUnit)}.interval"""

    interval3
  }

  private def  nonNull(a: Seq[String]): Seq[String] = if(a == null) Nil else a.toList
}

case class Attribute(name: String, interval: String, baseUnit: String, notExact: Boolean, default: Boolean)

case class CanonicalizedLinearUnit(
                                  name: String,
                                  objectName: String,
                                  symbol: String,
                                  aliases: Seq[String],
                                  interval: String,
                                  notExact: Boolean,
                                  attributes: Seq[Attribute],
                                  referToSIUnit: Boolean)

class LinearUnitDefinitionJson(jsonFile: File, destDir: File, mainDir: File, subpackage: String)
    extends UnitDefinitionJson(jsonFile, destDir, mainDir, subpackage){

  val unitCategoryType: Class[_ >: LinearUnitCategory] = new TypeToken[LinearUnitCategory]() {}.getRawType

  val unitCategory: LinearUnitCategory = IO.reader(jsonFile, UTF8) { reader =>
    gson.fromJson(reader, unitCategoryType).asInstanceOf[LinearUnitCategory]
  }

  lazy val composites: Composites =
    if (this.unitCategory.composites != null){
      val (p, q) = this.unitCategory.composites.partition(_.contains('*'))
      val prod = p.map(s => s.split('*')).map(ss => (ss(0).trim, ss(1).trim))
      val quot = q.map(s => s.split('/')).map(ss => (ss(0).trim, ss(1).trim))
      Composites(prod, quot)
    }else{
      Composites(Nil, Nil)
    }

  protected def doGenerate(jsons: Seq[JsonResource]): Unit = {
    IO.writer(this.destFile, "", UTF8, append = false) { writer: io.BufferedWriter =>
      val spj = jsons.find(_.isInstanceOf[ScalePrefixJson]).get.asInstanceOf[ScalePrefixJson]
      val units = this.unitCategory._units.flatMap(_.canonicalizeAndExpandScalePrefixes(spj.scalePrefixes))

      writer.write(
        s"""package $packageName
           |
           |import spire.math.Real
           |import spire.math.Fractional
           |""".stripMargin)

      if (units.nonEmpty && id != "TimeSquared"){
        writer.write("import spire.implicits._\n")
      }

      writer.write(
        s"""
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

      val unitDefs = jsons.filter(_.isUnitDefinitionJson).map(_.asInstanceOf[UnitDefinitionJson])

      val mul: Seq[(UnitDefinitionJson, UnitDefinitionJson)] =
        unitDefs.flatMap{ ud =>
          ud.composites.products
            .filter(_._1 == this.id)
            .map(p => (searchUnitDefinition(p._2, unitDefs), ud))
        }

      val div: Seq[(UnitDefinitionJson, UnitDefinitionJson)] =
        unitDefs.flatMap{ ud =>
          ud.composites.quotients
            .filter(_._1 == this.id)
            .map(p => (searchUnitDefinition(p._2, unitDefs), ud))
        }

      mul.foreach{ p =>
        if (this.id == "Length" && p._1.id == "Length"){
          writer.write(
            """  def square: AreaUnit =
              |    new AreaUnit{
              |      override val name: String = LengthUnit.this.name + " squared"
              |      override val symbol: String = LengthUnit.this.symbol + "²"
              |      override val interval: Real = LengthUnit.this.interval**2
              |      override def aliases: Seq[String] = {
              |        val heads = if (LengthUnit.this.name == "metre") Seq("m2") else Nil
              |
              |        val symbols = LengthUnit.this.symbol +: LengthUnit.this.aliases
              |        val squares = symbols.map(_+".square")
              |        val prods = symbols.map(a => a+"*"+a)
              |
              |        heads ++: squares ++: prods
              |      }
              |
              |      override def *(lengthUnit: LengthUnit): VolumeUnit = {
              |        if (lengthUnit == LengthUnit.this){
              |          LengthUnit.this.cubic
              |        } else {
              |          super.*(lengthUnit)
              |        }
              |      }
              |    }
              |
              |  def cubic: VolumeUnit =
              |    new VolumeUnit{
              |      override val name: String = LengthUnit.this.name + " cubic"
              |      override val symbol: String = LengthUnit.this.symbol + "³"
              |      override val interval: Real = LengthUnit.this.interval**3
              |      override def aliases: Seq[String] = {
              |        val heads = if (LengthUnit.this.name == "metre") Seq("m3") else Nil
              |
              |        val symbols = LengthUnit.this.symbol +: LengthUnit.this.aliases
              |        val cubics = symbols.map(_+".cubic")
              |        val prods = symbols.map(a => a+"*"+a+"*"+a)
              |
              |        heads ++: cubics ++: prods
              |      }
              |    }
              |
              |  def *(lengthUnit: LengthUnit): AreaUnit =
              |    if(this == lengthUnit)
              |      square
              |    else
              |      new ProductUnit[AreaUnit, LengthUnit, LengthUnit](LengthUnit.this, lengthUnit) with AreaUnit
              |""".stripMargin)

        } else if (this.id == "Time" && p._1.id == "Time") {
          writer.write(
            s"""  import org.waman.multiverse.unit.mechanics.TimeSquaredUnit
              |
              |  def square: TimeSquaredUnit =
              |    new TimeSquaredUnit{
              |      override val name: String = TimeUnit.this.name + " squared"
              |      override val symbol: String = TimeUnit.this.symbol + "²"
              |      override val interval: Real = TimeUnit.this.interval**2
              |      override def aliases: Seq[String] = {
              |        val heads = if (TimeUnit.this.name == "second") Seq("s2", "sec²", "sec2") else Nil
              |
              |        val symbols = TimeUnit.this.symbol +: TimeUnit.this.aliases
              |        val squares = symbols.map(_+".square")
              |        val prods = symbols.map(a => a+"*"+a)
              |
              |        heads ++: squares ++: prods
              |      }
              |    }
              |
              |  def *(timeUnit: TimeUnit): TimeSquaredUnit =
              |    if(this == timeUnit)
              |      square
              |    else
              |      new ProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit](TimeUnit.this, timeUnit) with TimeSquaredUnit
              |""".stripMargin)
        } else {

          Seq(p._1, p._2).distinct.foreach{ ud =>
            if (ud.subpackage != "" && ud.subpackage != this.subpackage)
              writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.${ud.subpackage}.${ud.id}Unit\n""")
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
      }

      div.foreach{ p =>
        Seq(p._1, p._2).foreach{ ud =>
          if (ud.subpackage != "" && ud.subpackage != this.subpackage)
            writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.${ud.subpackage}.${ud.id}Unit\n""")
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
      generateUnitObjectCode(writer, id, this.unitCategory.SIUnit, units, unitDefs)
      writer.write("\n")
      generateUnitsCode(writer, id, units)
    }
  }

  private def searchUnitDefinition(id: String, unitDefs: Seq[UnitDefinitionJson]): UnitDefinitionJson =
    unitDefs.find(_.id == id) match {
      case Some(ud) => ud
      case _ => throw new RuntimeException(s"""Unknown unit appears: $id""")
    }

  private def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)

  private def generateAttributesCode(writer: BufferedWriter, id: String, units: Seq[CanonicalizedLinearUnit]): Boolean = {
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
                                      writer: BufferedWriter, id: String, siUnit: String, units: Seq[CanonicalizedLinearUnit],
                                      unitDefs: Seq[UnitDefinitionJson]): Unit = {

    writer.write(s"object ${id}UnitObjects{\n")

    if(units.exists(u => u.interval.contains("Constants")))
      writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.Constants\n\n""")

    //***** SI Unit *****
    if (siUnit.contains('*') || siUnit.contains('/')){
      val GenerationUtil.regCompositeUnit(first, op, second) = siUnit

      List(first, second).distinct.map(searchUnitDefinition(_, unitDefs)).filterNot(_.subpackage == this.subpackage).foreach{ lu =>
        writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.${lu.subpackage}.${lu.id}UnitObjects\n""")
      }

      writer.write(
        s"""
           |  val getSIUnit: ${id}Unit = ${first}UnitObjects.getSIUnit $op ${second}UnitObjects.getSIUnit
           |
           |""".stripMargin)
    }else{
      writer.write(
        s"""
           |  def getSIUnit: ${id}Unit = $siUnit
           |
           |""".stripMargin)
    }

    //***** Unit Objects *****
    units.foreach{
      case u if u.referToSIUnit =>
        writer.write(s"""  val ${u.objectName}: ${id}Unit = getSIUnit\n""")

      case u =>
        val aliases =
          if (u.aliases.isEmpty) "Nil"
          else u.aliases.mkString("Seq(\"", "\", \"", "\")")

        val notExact =
          if (!u.notExact) ""
          else " with NotExact"

        // final case object metre extends DefaultLengthUnit("metre", "m", Nil, r"1")
        writer.write(
          s"""  final object ${u.objectName} extends Default${id}Unit""" +
            s"""("${u.name}", "${u.symbol}", $aliases, ${u.interval})$notExact\n""")
    }

    writer.write(
      s"""
         |  def getUnits: Seq[${id}Unit] =
         |    ${units.map(_.objectName).mkString("Seq(", ", ", ")")}
         |}
         |
         |""".stripMargin)
  }

  private def generateUnitsCode(
                                 writer: BufferedWriter, id: String, units: Seq[CanonicalizedLinearUnit]): Unit = {

    writer.write(s"""object ${id}Units{\n""")

    units.filterNot(_.name.contains("(")).foreach { u =>
      val sym = escapeSymbol(u.symbol)

      // def m: LengthUnit = LengthUnitObjects.metre
      writer.write(s"""  def $sym: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")

      // def xu(a: xunitAttribute): LengthUnit = a match {
      //   case CuKα1 => LengthUnitObjects.`xunit(CuKα1)`
      //   case MoKα1 => LengthUnitObjects.`xunit(MoKα1)`
      // }
      if (u.attributes.nonEmpty) {
        writer.write(s"""  def $sym(a: ${u.objectName}Attribute): ${id}Unit = a match { \n""")
        u.attributes.foreach { a =>
          writer.write(s"""    case ${id}Attributes.${a.name} => ${id}UnitObjects.`${u.objectName}(${a.name})`\n""")
        }
        writer.write("  }\n")
      }

      u.aliases.foreach { al =>
        val als = escapeSymbol(al)
        writer.write(s"""  def $als: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")

        // def nmi(a: nautical_mileAttribute): LengthUnit = NM(a)
        if (u.attributes.nonEmpty && !als.contains("_")) {  // the second condition is for removing ones like cal_IT(IT)
          writer.write(s"""  def $als(a: ${u.objectName}Attribute): ${id}Unit = $sym(a)\n""")
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

  def escapeSymbol(s: String): String =
    if (s.contains('²') || s.contains('³') || s == "°") s"""`$s`"""
    else s
}