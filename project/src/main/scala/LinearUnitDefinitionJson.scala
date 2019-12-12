import java.io
import java.io.{BufferedWriter, File}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class LinearUnitCategory(
                               SIUnit: String, dimension: Dimension, composites: Array[String], units: Array[LinearUnit]){
  def _units: Seq[LinearUnit] = if (this.units != null) this.units else Nil
}

case class Dimension(M:Int, L: Int, T: Int, I: Int, Θ: Int, N: Int, J: Int){
  def getEntries: Map[String, Int] =
    Map("M" -> M, "L" -> L, "T" -> T, "I" -> I, "Θ" -> Θ, "N" -> N, "J" -> J).filter(_._2 != 0)
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
  import GenerationUtil._

  def _aliases: Seq[String] =
    if (this.aliases != null) this.aliases else Nil

  lazy val _excludePrefixes: Seq[String] =
    if (this.excludePrefixes != null) this.excludePrefixes else Nil

  def canonicalizeAndExpandScalePrefixes(
      prefixes: Seq[ScalePrefix], unitDefs: Seq[LinearUnitDefinitionJson]): Seq[CanonicalizedLinearUnit] = {

    if (this.name == "square" || this.name == "cubic") {
      // For Area, Volume, and TimeSquared units:
      //   {"name": "square", "baseUnit": "Time.second", "scalePrefixes": true, "excludePrefixes":[...]}
      val baseUnits = this.baseUnit.split('.')  // ("Length", "metre")
      val (baseId, baseName) = (baseUnits(0), baseUnits(1))
      val (sup, suf) = this.name match {
        case "square" => ("²", "2")
        case "cubic"   => ("³", "3")
        case _ => throw new RuntimeException("Unknown power appears: " + this.name)
      }

      val targetUnit = searchUnitDefinition(baseId, unitDefs).unitCategory._units.find(_.name == baseName).get
      val baseSymbol = targetUnit.symbol
      val baseAliases = targetUnit._aliases

      def addSuffices(s: String): Seq[String] = Seq(s+sup, s+suf)

      val ali = (baseSymbol +: baseAliases).flatMap(addSuffices)  // Seq("s²", "s2", "sec²", "sec2")

      // {"name":"square second", "objectName":"square_second", "symbol":"s²", "aliases":["s2", "sec²", "sec2"],
      //   "interval":(null), "baseUnit":"Time.second", ..., "defineAsVal":true}
      val head = CanonicalizedLinearUnit(
        this.name+" "+baseName, this.name+"_"+baseName, baseSymbol+sup, ali.tail,
        null, this.baseUnit, this.notExact, Nil, isPower=true)

      if (!this.scalePrefixes) {
        return Seq(head)
      }

      val tail = prefixes.filterNot(p => this._excludePrefixes.contains(p.prefix)).map{ p =>
        val prefixedBaseUnit = this.baseUnit.replace(".", s""".${p.name}""")  // Time.millimetre
        val al = (p.prefix +: p._aliases).flatMap(ps => ali.map(a => ps + a)).tail
        CanonicalizedLinearUnit(
          this.name+" "+p.name+baseName, this.name+"_"+p.name+baseName, p.prefix+baseSymbol+sup, al,
          null, prefixedBaseUnit, this.notExact, Nil, isPower=true)
      }

      return head +: tail
    }

    val interval = makeIntervalExpression(this.interval, this.baseUnit)
    val aliases = this._aliases

    if(scalePrefixes){
      // In this case, always name == objectName
      val head = CanonicalizedLinearUnit(this.name, this.name, this.symbol, aliases,
        interval, null, this.notExact, Nil, isPower=false)

      val nm = this.symbol +: aliases
      val tail = prefixes.filterNot(p => this._excludePrefixes.contains(p.prefix)).map{ p =>
          val al = (p.prefix +: p._aliases).flatMap(ps => nm.map(ns => ps + ns)).tail
          val name = p.name + this.name
          CanonicalizedLinearUnit(name, name, p.prefix + this.symbol, nonNull(al),
            s"""$interval * r"${p.scale}"""", null, this.notExact, Nil, isPower=false)
        }

      head +: tail
    }else{
      val atts =
        if(this.attributes == null) Nil
        else this.attributes.toList

      val u = CanonicalizedLinearUnit(
        this.name, toObjectName(this.name), this.symbol, aliases, interval, null, this.notExact, atts, isPower=false)

      val us = atts.map{ a =>
        val name = s"${u.name}(${a.name})"
        CanonicalizedLinearUnit(
          name, toObjectName(name), s"${u.symbol}(${a.name})", aliases.map(al => s"$al(${a.name})"),
          makeIntervalExpression(a.interval, a.baseUnit), null, a.notExact, Nil, isPower=false)
      }

      u +: us
    }
  }

  private def makeIntervalExpression(interval: String, baseUnit: String): String =
    if (baseUnit == null) {
      if (interval == null) "1"
      else refineNumbers(interval)

    } else {
      val baseUnitInterval = refineUnitNames(baseUnit)
      if (interval == null) baseUnitInterval
      else s"""${refineNumbers(interval)} * $baseUnitInterval"""
    }

  private def  nonNull(a: Seq[String]): Seq[String] = if(a == null) Nil else a.toList
}

case class Attribute(name: String, interval: String, baseUnit: String, notExact: Boolean)

case class CanonicalizedLinearUnit(
                                    name: String,
                                    objectName: String,
                                    symbol: String,
                                    aliases: Seq[String],
                                    interval: String,
                                    baseUnit: String,
                                    notExact: Boolean,
                                    attributes: Seq[Attribute],
                                    isPower: Boolean){  // true for square or cubic
  require(if (isPower) interval == null else baseUnit == null)
}

class LinearUnitDefinitionJson(jsonFile: File, destDir: File, mainDir: File, subpackage: String)
    extends UnitDefinitionJson(jsonFile, destDir, mainDir, subpackage){

  import GenerationUtil._

  val unitCategoryType: Class[_ >: LinearUnitCategory] = new TypeToken[LinearUnitCategory]() {}.getRawType

  val unitCategory: LinearUnitCategory = IO.reader(jsonFile, utf8) { reader =>
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
    IO.writer(this.destFile, "", utf8, append = false) { writer: io.BufferedWriter =>
      val spj = extractResources(jsons, classOf[ScalePrefixJson]).head
      val unitDefs = extractResources(jsons, classOf[UnitDefinitionJson])
      val linearUnitDefs = extractResources(unitDefs, classOf[LinearUnitDefinitionJson])

      val units = this.unitCategory._units.flatMap(_.canonicalizeAndExpandScalePrefixes(spj.scalePrefixes, linearUnitDefs))

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

      writer.write(
        s"""package $packageName
           |
           |import spire.math.Real
           |import spire.math.Fractional
           |""".stripMargin)

      if (units.nonEmpty && this.id != "TimeSquared" && this.id != "VolumeFlow"){
        writer.write("import spire.implicits._\n")
      }

      writer.write(
        s"""import $rootPackage._
           |
           |""".stripMargin)

      Seq.concat(mul, div).flatMap(p => Seq(p._1, p._2)).distinct.foreach{ ud =>
        if (ud.subpackage != this.subpackage)
          writer.write(
            s"""import $rootPackage.unit.${ud.subpackage}.${ud.id}
               |import $rootPackage.unit.${ud.subpackage}.${ud.id}Unit
               |
               |""".stripMargin)
      }

      writer.write(
        s"""class $id[A: Fractional](val value: A, val unit: ${id}Unit)
           |    extends LinearQuantity[$id[A], A, ${id}Unit] {
           |
           |  override protected def newQuantity(value: A, unit: ${id}Unit): $id[A] = new $id(value, unit)
           |""".stripMargin)

      mul.foreach(generateQuantityMultiplication(writer, _))

      div.foreach{ p =>
        val denoType = p._1.id
        val resultType = p._2.id
        val denoId = headToLower(denoType)
        writer.write(
          s"""  def /($denoId: $denoType[A]): $resultType[A] = new $resultType(this.value / $denoId.value, this.unit / $denoId.unit)
             |
             |""".stripMargin)
      }

      writer.write(
        s"""}
           |
           |trait ${id}Unit extends LinearUnit[${id}Unit]{
           |
           |  override def getSIUnit: ${id}Unit = ${id}Unit.getSIUnit
           |  override def dimension: Map[DimensionSymbol, Int] = ${id}Unit.dimension
           |
           |""".stripMargin)

      mul.foreach(generateUnitMultiplication(writer, _))

      div.foreach{ p =>
        val denoType = p._1.id
        val resultType = p._2.id
        val denoId = headToLower(denoType)
        writer.write(
          s"""  def /(${denoId}Unit: ${denoType}Unit): ${resultType}Unit =
             |    new QuotientUnit[${resultType}Unit, ${id}Unit, ${denoType}Unit](${id}Unit.this, ${denoId}Unit) with ${resultType}Unit
             |
             |""".stripMargin)
      }

      writer.write("}\n\n")

      generateXxxUnitCode(writer, units, unitDefs)
      writer.write("\n")
      generateXxxAttributesCode(writer, units)
      writer.write("\n")
      generateXxxUnitObjectsCode(writer, units, unitDefs)
      writer.write("\n")
      generateXxxUnitsCode(writer, units)
    }
  }

  protected def generateQuantityMultiplication(writer: BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    val secondType = p._1.id
    val resultType = p._2.id
    val secondId = headToLower(secondType)
    writer.write(
      s"""  def *($secondId: ${secondType}[A]): $resultType[A] = new $resultType(this.value * $secondId.value, this.unit * $secondId.unit)
         |
         |""".stripMargin)
  }

  protected def generateUnitMultiplication(writer: io.BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    val secondType = p._1.id
    val resultType = p._2.id
    val secondId = headToLower(secondType)
    writer.write(
      s"""  def *(${secondId}Unit: ${secondType}Unit): ${resultType}Unit =
         |    new ProductUnit[${resultType}Unit, ${id}Unit, ${secondType}Unit](${id}Unit.this, ${secondId}Unit) with ${resultType}Unit
         |
         |""".stripMargin)
  }

  private def generateXxxUnitCode(
                                    writer: BufferedWriter, units: Seq[CanonicalizedLinearUnit], unitDefs: Seq[UnitDefinitionJson]): Unit = {

    writer.write(
      s"""object ${id}Unit{
         |""".stripMargin)

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

      List(first, second).distinct.map(searchUnitDefinition(_, unitDefs)).filterNot(_.subpackage == this.subpackage).foreach { ud =>
        writer.write(s"""  import $rootPackage.unit.${ud.subpackage}.${ud.id}Unit\n""".stripMargin)
      }

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

  private def generateXxxAttributesCode(writer: BufferedWriter, units: Seq[CanonicalizedLinearUnit]): Boolean = {
    val attUnits = units.filter(_.attributes.nonEmpty)
    if (attUnits.isEmpty) return false

    attUnits.foreach { u =>
      writer.write(
        s"""sealed trait ${u.objectName}Attribute
           |""".stripMargin)
    }
    writer.write("\n")

    // [gregorian: Seq(month, year, decade, ...), julian: Seq(year, decade, ...), ...]
    val map = attUnits.flatMap(u => u.attributes.map(a => (u.objectName, a.name)))
      .groupBy(_._2)
      .mapValues(v => v.map(_._1))
    writer.write(s"object ${id}Attributes{\n")
    map.foreach { u =>
      writer.write(s"""  final object ${u._1} extends ${u._2.map(_ + "Attribute").mkString(" with ")}\n""")
    }
    writer.write("}\n")
    true
  }

  private def generateXxxUnitObjectsCode(
       writer: BufferedWriter, units: Seq[CanonicalizedLinearUnit], unitDefs: Seq[UnitDefinitionJson]): Unit = {

    writer.write(
      s"""class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
         |  extends ${id}Unit
         |
         |""".stripMargin)

    writer.write(s"object ${id}UnitObjects{\n")

    if(units.exists(u => u.interval != null && u.interval.contains("Constants")))
      writer.write(s"""  import $rootPackage.unit.Constants\n""")

    this.unitCategory._units.map(_.baseUnit).filterNot(_ == null)
        .flatMap(extraUnitsInBaseUnit).distinct.map(searchUnitDefinition(_, unitDefs)).foreach{ ud =>
      if (ud.subpackage != this.subpackage)
        writer.write(s"""  import $rootPackage.unit.${ud.subpackage}.${ud.id}UnitObjects\n""")
    }

    if(id == "TimeSquared")
      writer.write(s"""  import $rootPackage.unit.basic.TimeUnitObjects\n""")

    writer.write("\n")

    //***** Unit Objects *****
    units.foreach{
      case u if u.isPower =>
        // val square_metre: AreaUnit = LengthUnitObjects.metre.squared
        val baseUnits = u.baseUnit.split('.')  // baseUnit: Length.metre
        val power = if (u.name.startsWith("cubic")) "cubic" else "squared"
        writer.write(s"""  val ${u.objectName}: ${id}Unit = ${baseUnits(0)}UnitObjects.${baseUnits(1)}.$power\n""")

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

    writer.write("}\n")
  }

  private def generateXxxUnitsCode(writer: BufferedWriter, units: Seq[CanonicalizedLinearUnit]): Unit = {

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
          writer.write(s"""  def $als(a: ${u.objectName}Attribute): ${id}Unit = $sym(a)\n\n""")
        }
      }
    }

    writer.write("}")
  }

  def escapeSymbol(s: String): String =
    if (s.contains('²') || s.contains('³') || s == "°") s"""`$s`"""
    else s
}

class LengthUnitDefinitionJson(jsonFile: File, destDir: File, mainDir: File, subpackage: String)
    extends LinearUnitDefinitionJson(jsonFile, destDir, mainDir, subpackage){

  override protected def generateQuantityMultiplication(
      writer: BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {

    super.generateQuantityMultiplication(writer, p)
    if (p._1.id == "Length")
      writer.write(
        s"""  def squared: Area[A] = this * this
           |  def cubic: Volume[A] = this * this * this
           |
           |""".stripMargin)
  }

  override protected def generateUnitMultiplication(
      writer: io.BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit =
    if (p._1.id == "Length") {
      writer.write(
        """  def squared: AreaUnit =
          |    new AreaUnit{
          |      override val name: String = "square " + LengthUnit.this.name
          |      override val symbol: String = LengthUnit.this.symbol + "²"
          |      override val interval: Real = LengthUnit.this.interval**2
          |      override def aliases: Seq[String] = {
          |        val heads = if (LengthUnit.this.name == "metre") Seq("m2") else Nil
          |
          |        val symbols = LengthUnit.this.symbol +: LengthUnit.this.aliases
          |        val squares = symbols.map(_+".squared")
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
          |      override val name: String = "cubic " + LengthUnit.this.name
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
          |      this.squared
          |    else
          |      new ProductUnit[AreaUnit, LengthUnit, LengthUnit](LengthUnit.this, lengthUnit) with AreaUnit
          |
          |""".stripMargin)
    }else{
      super.generateUnitMultiplication(writer, p)
    }
}

class TimeUnitDefinitionJson(jsonFile: File, destDir: File, mainDir: File, subpackage: String)
    extends LinearUnitDefinitionJson(jsonFile, destDir, mainDir, subpackage){

  override protected def generateQuantityMultiplication(
     writer: BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    super.generateQuantityMultiplication(writer, p)
    if (p._1.id == "Time")
      writer.write(s"""  def squared: TimeSquared[A] = this * this\n""")
  }

  override protected def generateUnitMultiplication(
      writer: io.BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit =
    if (p._1.id == "Time") {
      writer.write(
        s"""  def squared: TimeSquaredUnit =
           |    new TimeSquaredUnit{
           |      override val name: String = TimeUnit.this.name + " squared"
           |      override val symbol: String = TimeUnit.this.symbol + "²"
           |      override val interval: Real = TimeUnit.this.interval**2
           |      override def aliases: Seq[String] = {
           |        val heads = if (TimeUnit.this.name == "second") Seq("s2", "sec²", "sec2") else Nil
           |
           |        val symbols = TimeUnit.this.symbol +: TimeUnit.this.aliases
           |        val squares = symbols.map(_+".squared")
           |        val prods = symbols.map(a => a+"*"+a)
           |
           |        heads ++: squares ++: prods
           |      }
           |    }
           |
           |  def *(timeUnit: TimeUnit): TimeSquaredUnit =
           |    if(this == timeUnit)
           |      this.squared
           |    else
           |      new ProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit](TimeUnit.this, timeUnit) with TimeSquaredUnit
           |""".stripMargin)
    }else{
      super.generateUnitMultiplication(writer, p)
    }
}