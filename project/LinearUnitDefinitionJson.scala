import java.io.{File, BufferedWriter => BW}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class LinearUnitCategory(description: String, SIUnit: String, dimension: Dimension,
                              composites: Array[String], convertibles: Array[RawConvertible], units: Array[RawLinearUnit])
    extends UnitCategory[RawLinearUnit, LinearUnit]{

  lazy val _composites: Composites =
    if (this.composites != null){
      val (p, q) = this.composites.partition(_.contains('*'))
      val prod = p.map(s => s.split('*')).map(ss => (ss(0).trim, ss(1).trim))
      val quot = q.map(s => s.split('/')).map(ss => (ss(0).trim, ss(1).trim))
      Composites(prod, quot)
    }else{
      Composites(Nil, Nil)
    }
}

case class Composites(products: Seq[(String, String)], quotients: Seq[(String, String)])

// ex)
//{"name":"electronvolt", "symbol":"eV", "interval":"Constants.ElementaryCharge",
//   scalePrefixes":true, "notExact":true},
case class RawLinearUnit(name: String, symbol: String, aliases: Array[String], interval: String, baseUnit: String,
                         scalePrefixes: Boolean, excludePrefixes: Array[String], notExact: Boolean,
                         attributes: Array[Attribute], description: String) extends RawUnitInfo[LinearUnit]{ self =>
  import GenerationUtil._

  lazy val _symbol: String =
    if (this.symbol == null) {
      toObjectName(this.name)
    } else {
      if (this.symbol == this.name) println(s"symbol attribute is reduntant: $name")
      this.symbol
    }

  lazy val _attributes: Seq[Attribute] = GenerationUtil.toSeq(this.attributes)

  override def expandScalePrefixesAndAttributes(jsons: JsonResources): Seq[LinearUnit] = {
    val prefixes = jsons.scalePrefixJson.scalePrefixes

    if(scalePrefixes){
      val head = LinearUnit(this.name, toObjectName(this.name), this._symbol, this._aliases,
                                      this.interval, this.baseUnit, this.notExact, Nil, this.description)

      abstract class PrefixedUnitsBuilder{
        def prefixedName(name: String, p: ScalePrefix): String
        def prefixedInterval(interval: String, p: ScalePrefix): String
        def prefixedBaseUnit(baseUnit: String, p: ScalePrefix): String

        def build(): Seq[LinearUnit] =
          prefixes.filterNot(p => _excludePrefixes.contains(p.prefix)).map { p =>
            val al = (p.prefix +: p._aliases).flatMap(ps => symbols.map(ps + _)).tail
            val pname = prefixedName(name, p)
            LinearUnit(pname, toObjectName(pname), p.prefix+_symbol, al,
              prefixedInterval(self.interval, p), prefixedBaseUnit(self.baseUnit, p), self.notExact, Nil, null)
          }
      }

      class DefaultPrefixedUnitsBuilder extends PrefixedUnitsBuilder{
        override def prefixedName(name: String, p: ScalePrefix): String =  p.name + name
        override def prefixedInterval(interval: String, p: ScalePrefix): String =
          if (interval != null) s"""$interval * ${p.scale}""" else p.scale
        override def prefixedBaseUnit(baseUnit: String, p: ScalePrefix): String = baseUnit  // do nothing (reach only in Entropy)
      }

      class LengthPoweredPrefixedUnitsBuilder(baseUnitName: String) extends PrefixedUnitsBuilder{
        private val powerName = baseUnitName.split(' ')(0)
        // square metre -> square millimetre
        override def prefixedName(name: String, p: ScalePrefix): String =  s"$powerName ${p.name}metre"
        override def prefixedInterval(interval: String, p: ScalePrefix): String = {
          require(interval == null, "LengthPoweredPrefixedUnitsBuilder#prefixedInterval")
          null
        }
        // LengthUnitObjects.metre**2 -> LengthUnitObjects.millimetre**2
        override def prefixedBaseUnit(baseUnit: String, p: ScalePrefix): String = baseUnit.replace("metre", p.name+"metre")
      }

      class TimeSquaredUnitsBuilder extends PrefixedUnitsBuilder{
        // second squared -> millisecond squared
        override def prefixedName(name: String, p: ScalePrefix): String =  p.name + name
        override def prefixedInterval(interval: String, p: ScalePrefix): String = {
          require(interval == null, "TimeSquaredUnitsBuilder#prefixedInterval")
          null
        }
        // TimeUnitObjects.second**2 -> TimeUnitObjects.millisecond**2
        override def prefixedBaseUnit(baseUnit: String, p: ScalePrefix): String = baseUnit.replace("second", p.name+"second")
      }

      val builder = this.name match {
        case "square metre" | "cubic metre" => new LengthPoweredPrefixedUnitsBuilder(this.name)
        case "second squared" => new TimeSquaredUnitsBuilder
        case _ => new DefaultPrefixedUnitsBuilder
      }

      head +: builder.build()

    } else{
      val u = toLinearUnit

      val us = this._attributes.map{ a =>
        val _name = s"${u.name}(${a.name})"
        val _aliases = this._aliases.map(al => s"$al(${a.name})") ++: a._aliases
        LinearUnit(
          _name, toObjectName(_name), s"${u.symbol}(${a.name})", _aliases,
          a.interval, a.baseUnit, a.notExact, Nil, a.description)
      }

      u +: us
    }
  }

  def toLinearUnit: LinearUnit =
    LinearUnit(this.name, toObjectName(this.name), this._symbol, this._aliases,
      this.interval, this.baseUnit, this.notExact, this._attributes, this.description)
}

case class Attribute(name: String, aliases: Array[String], interval: String, baseUnit: String, notExact: Boolean, description: String){
  lazy val _aliases: Seq[String] = GenerationUtil.toSeq(this.aliases)
}

case class LinearUnit(name: String, objectName: String, symbol: String, aliases: Seq[String],
                      interval: String, baseUnit: String, notExact: Boolean, attributes: Seq[Attribute], description: String)
    extends UnitInfo{
  require(this.attributes != null)

  lazy val intervalExpression: String = makeIntervalExpression(this.interval, this.baseUnit)

  import GenerationUtil._

  private def makeIntervalExpression(interval: String, baseUnit: String): String =
    if (baseUnit == null) {
      if (interval == null) "1" else refineNumbers(interval)

    } else {
      val baseUnitInterval = refineUnitNamesInBaseUnit(baseUnit)
      if (interval == null) baseUnitInterval
      else s"""${refineNumbers(interval)} * $baseUnitInterval"""
    }
}

class OperationInfo(id: String, jsons: JsonResources){

  val mul: Seq[(UnitDefinitionJson, UnitDefinitionJson)] = jsons.linearUnitDefs.flatMap { lud =>
    lud.unitCategory._composites.products.filter(_._1 == id)
      .map(p => (jsons.searchUnitDefinition(p._2), lud))
  }

  val div: Seq[(UnitDefinitionJson, UnitDefinitionJson)]  = jsons.linearUnitDefs.flatMap { lud =>
    lud.unitCategory._composites.quotients.filter(_._1 == id)
      .map(p => (jsons.searchUnitDefinition(p._2), lud))
  }

  def ops: Seq[(UnitDefinitionJson, UnitDefinitionJson)] = this.mul ++: this.div
}

class LinearUnitDefinitionJson(jsonFile: File, destDir:  File, subpackage: String)
    extends UnitDefinitionJsonAdapter[LinearUnitCategory, RawLinearUnit, LinearUnit, OperationInfo](
      "Linear", jsonFile, destDir, subpackage) {

  import GenerationUtil._

  val unitCategoryType: Class[_ >: LinearUnitCategory] = new TypeToken[LinearUnitCategory]() {}.getRawType

  val unitCategory: LinearUnitCategory = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitCategoryType).asInstanceOf[LinearUnitCategory]
  }

  override protected def getUnits(jsons: JsonResources): Seq[LinearUnit] =
    this.unitCategory._units.flatMap(_.expandScalePrefixesAndAttributes(jsons))

  override protected def createOptions(jsons: JsonResources): OperationInfo = new OperationInfo(this.id, jsons)

  override def generateExtraGlobalImports(writer: BW, jsons: JsonResources, op: OperationInfo): Unit = {

    val ops = op.ops.flatMap(p => Seq(p._1, p._2)).map(_.id)
    foreachUnitDefinition(ops, jsons){ ud =>
      if (ud.subpackage != this.subpackage) {
        if (this.id == "Energy" && (ud.id == "AbsoluteTemperature" || ud.id == "Mass")) {
          // TODO: remove unused imports in Energy
          writer.write(
            s"""
               |import $rootPackage.unit.${ud.subpackage}.${ud.id}Unit
               |
               |""".stripMargin)
        } else {
          writer.write(
            s"""
               |import $rootPackage.unit.${ud.subpackage}.${ud.id}
               |import $rootPackage.unit.${ud.subpackage}.${ud.id}Unit
               |
               |""".stripMargin)
        }
      }
    }
  }

  override protected def parentQuantityDecl: String = s"""LinearQuantity[$id[A], A, ${id}Unit]"""

  override protected def generateQuantityExtraContents(
      writer: BW, jsons: JsonResources, op: OperationInfo, spireImplicitsOutputted: Boolean): Unit = {

    if (!spireImplicitsOutputted && op.ops.nonEmpty) {
      writer.write("  import spire.implicits._\n\n")
    }

    writer.write(
      s"""  override protected def newQuantity(value: A, unit: ${id}Unit): $id[A] = new $id(value, unit)
         |""".stripMargin)

    op.mul.foreach(generateQuantityMultiplication(writer, _))

    op.div.foreach { p =>
      val denoType = p._1.id
      val resultType = p._2.id
      val denoId = headToLower(denoType)
      writer.write(
        s"""
           |  def /($denoId: $denoType[A]): $resultType[A] = new $resultType(this.value / $denoId.value, this.unit / $denoId.unit)
           |""".stripMargin)
    }
  }

  protected def generateQuantityMultiplication(writer: BW, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    val secondType = p._1.id
    val resultType = p._2.id
    val secondId = headToLower(secondType)
    writer.write(
      s"""
         |  def *($secondId: $secondType[A]): $resultType[A] = new $resultType(this.value * $secondId.value, this.unit * $secondId.unit)
         |""".stripMargin)
  }

  override protected def generateUnitTraitExtraContents(writer: BW, jsons: JsonResources, op: OperationInfo): Unit = {
    op.mul.foreach(generateUnitMultiplication(writer, _))

    op.div.foreach { p =>
      val denoType = p._1.id
      val resultType = p._2.id
      val denoId = headToLower(denoType)
      writer.write(
        s"""
           |  def /(${denoId}Unit: ${denoType}Unit): ${resultType}Unit =
           |    new QuotientUnit[${resultType}Unit, ${id}Unit, ${denoType}Unit](${id}Unit.this, ${denoId}Unit) with ${resultType}Unit
           |""".stripMargin)
    }
  }

  protected def generateUnitMultiplication(writer: BW, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    val secondType = p._1.id
    val resultType = p._2.id
    val secondId = headToLower(secondType)
    writer.write(
      s"""
         |  def *(${secondId}Unit: ${secondType}Unit): ${resultType}Unit =
         |    new ProductUnit[${resultType}Unit, ${id}Unit, ${secondType}Unit](${id}Unit.this, ${secondId}Unit) with ${resultType}Unit
         |""".stripMargin)
  }

  override protected def generateImplsOfUnitTrait(writer: BW): Unit = {
    writer.write(
      s"""/** For no aliase or user defined units */
         |class Simple${id}Unit(val name: String, val symbol: String, val interval: Real) extends ${id}Unit {
         |  override def aliases: Seq[String] = Nil
         |}
         |
         |/** For units which has aliases */
         |class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
         |  extends ${id}Unit
         |
         |""".stripMargin)
  }

  override protected def generateUnitCaseObject(writer: BW, unit: LinearUnit): Unit = {
    val notExact = if (!unit.notExact) "" else " with NotExact"
    val desc = if (unit.description != null) {
      s""" with Description {
         |    def description: String = "${unit.description}"
         |  }""".stripMargin
    } else ""

    if (unit.aliases.isEmpty){
      // final case object metre extends SimpleLengthUnit("metre", "m", Nil, r"1")
      writer.write(
        s"""  final case object ${unit.objectName} extends Simple${id}Unit""" +
          s"""("${unit.name}", "${unit.symbol}", ${unit.intervalExpression})$notExact$desc\n""")
    }else{
      // final case object metre extends DefaultLengthUnit("micrometre", "μm", Seq("mcm"), r"1e-6")
      val aliases = unit.aliases.filterNot(isOptionalAliase).mkString("Seq(\"", "\", \"", "\")")
      writer.write(
        s"""  final case object ${unit.objectName} extends Default${id}Unit""" +
          s"""("${unit.name}", "${unit.symbol}", $aliases, ${unit.intervalExpression})$notExact$desc\n""")
    }
  }

  override protected def generateAttributes(writer: BW, jsons: JsonResources, units: Seq[LinearUnit]): Unit = {
    val attUnits = getUnitsWithAttributes(jsons, units)
    if (attUnits.isEmpty) return

    attUnits.foreach { u =>
      writer.write(
        s"""sealed trait ${u.objectName}Attribute
           |""".stripMargin)
    }
    writer.write("\n")
  }
}

class LengthUnitDefinitionDefinitionJson(jsonFile: File, destDir: File, subpackage: String)
    extends LinearUnitDefinitionJson(jsonFile, destDir, subpackage){

  override protected def attributeContainerID: String = "MetricAttributes"

  override protected def generateQuantityMultiplication(writer: BW, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {

    super.generateQuantityMultiplication(writer, p)
    if (p._1.id == "Length")
      writer.write(
        s"""  def squared: Area[A] = this * this
           |  def cubic: Volume[A] = this * this * this
           |
           |""".stripMargin)
  }

  override protected def generateUnitMultiplication(writer:BW, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit =
    if (p._1.id == "Length") {
      writer.write(
        """  def squared: AreaUnit =
          |    new AreaUnit{
          |      override val name: String = "square " + LengthUnit.this.name
          |      override val symbol: String = LengthUnit.this.symbol + "²"
          |      override val interval: Real = LengthUnit.this.interval**2
          |      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".squared")
          |
          |      override def *(lengthUnit: LengthUnit): VolumeUnit = {
          |        if (lengthUnit == LengthUnit.this)
          |          LengthUnit.this.cubic
          |        else
          |          super.*(lengthUnit)
          |      }
          |    }
          |
          |  def cubic: VolumeUnit =
          |    new VolumeUnit{
          |      override val name: String = "cubic " + LengthUnit.this.name
          |      override val symbol: String = LengthUnit.this.symbol + "³"
          |      override val interval: Real = LengthUnit.this.interval**3
          |      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".cubic")
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

  override protected def getUnitsWithAttributes(jsons: JsonResources, units: Seq[LinearUnit]): Seq[LinearUnit] =
    jsons.linearUnitDefs.filter(ud => ud.id == "Length" || ud.id == "Area" || ud.id == "Volume")
      .flatMap(_.unitCategory._units.filter(_._attributes.nonEmpty).map(_.toLinearUnit))

  override protected def generateAttributes(writer: BW, jsons: JsonResources, units: Seq[LinearUnit]): Unit = {
    super.generateAttributes(writer, jsons, units)

  //  object MetricAttributes{
  //    final object MoKα1 extends xunitAttribute
  //    final object Adm extends nautical_mileAttribute
  //    ...
  //  }
    val attUnits = getUnitsWithAttributes(jsons, units)
    writer.write("object MetricAttributes{\n")
    super.generateAttributeObjects(writer, jsons, attUnits)
    writer.write("}\n\n")
  }

  override protected def generateAttributeObjects(writer: BW, jsons: JsonResources, units: Seq[LinearUnit]): Unit = ()
}

class LengthPoweredUnitDefinitionDefinitionJson(jsonFile: File, destDir: File, subpackage: String)
    extends LinearUnitDefinitionJson(jsonFile, destDir, subpackage){

  override protected def attributeContainerID: String = "MetricAttributes"
  override protected def generateAttributes(writer: BW, jsons: JsonResources, units: Seq[LinearUnit]): Unit = ()

  override protected def generateAttributeObjects(writer: BW, jsons: JsonResources, units: Seq[LinearUnit]): Unit = ()
}

class TimeUnitDefinitionJson(jsonFile: File, destDir: File, subpackage: String)
    extends LinearUnitDefinitionJson(jsonFile, destDir, subpackage){

  override protected def generateQuantityMultiplication(writer: BW, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    super.generateQuantityMultiplication(writer, p)
    if (p._1.id == "Time")
      writer.write(s"""  def squared: TimeSquared[A] = this * this\n""")
  }

  override protected def generateUnitMultiplication(writer: BW, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit =
    if (p._1.id == "Time") {
      writer.write(
        s"""  def squared: TimeSquaredUnit =
           |    new TimeSquaredUnit{
           |      override val name: String = TimeUnit.this.name + " squared"
           |      override val symbol: String = TimeUnit.this.symbol + "²"
           |      override val interval: Real = TimeUnit.this.interval**2
           |      override def aliases: Seq[String] = TimeUnit.this.symbols.map(_+".squared")
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