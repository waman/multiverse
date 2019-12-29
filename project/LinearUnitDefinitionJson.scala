import java.io
import java.io.{BufferedWriter, File}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class LinearUnitCategory(SIUnit: String, dimension: Dimension, composites: Array[String],
                                           convertibles: Array[Convertible], private val units: Array[LinearUnit]){
  lazy val _units: Seq[LinearUnit] = if (this.units != null) this.units else Nil
  lazy val _convertibles: Seq[Convertible] = if (this.convertibles != null) this.convertibles else Nil
}

case class Dimension(M: Int, L: Int, T: Int, I: Int, Θ: Int, N: Int, J: Int){
  def getEntries: Map[String, Int] =
    Map("M" -> M, "L" -> L, "T" -> T, "I" -> I, "Θ" -> Θ, "N" -> N, "J" -> J).filter(_._2 != 0)
}

case class Convertible(target: String, from: String, to: String, factor: String)

// ex)
//{"name":"electronvolt", "symbol":"eV", "interval":"Constants.ElementaryCharge",
//   scalePrefixes":true, "notExact":true},
case class LinearUnit(name: String, symbol: String, private val aliases: Array[String], interval: String, baseUnit: String,
                     scalePrefixes: Boolean, private val excludePrefixes: Array[String], notExact: Boolean,
                      private val attributes: Array[Attribute]){
  import GenerationUtil._

  lazy val _aliases: Seq[String] = if (this.aliases != null) this.aliases else Nil
  lazy val symbols: Seq[String] = this.symbol +: this._aliases
  lazy val _excludePrefixes: Seq[String] = if (this.excludePrefixes != null) this.excludePrefixes else Nil
  lazy val _attributes: Seq[Attribute] = if (this.attributes != null) this.attributes.toList else Nil

  def canonicalizeAndExpandScalePrefixes(jsons: JsonResources): Seq[CanonicalizedLinearUnit] = {

    def makeIntervalExpression(interval: String, baseUnit: String): String =
      if (baseUnit == null) {
        if (interval == null) "1" else refineNumbers(interval)

      } else {
        val baseUnitInterval = refineUnitNamesInBaseUnit(baseUnit)
        if (interval == null) baseUnitInterval
        else s"""${refineNumbers(interval)} * $baseUnitInterval"""
      }

    val prefixes = jsons.scalePrefixJson.scalePrefixes

    val baseInterval = makeIntervalExpression(this.interval, this.baseUnit)

    if(scalePrefixes){
      val head = CanonicalizedLinearUnit(this.name, toObjectName(this.name), this.symbol, this._aliases,
                                                      baseInterval, null, this.notExact, Nil)

      abstract class PrefixedUnitsBuilder{
        def prefixedName(name: String, p: ScalePrefix): String
        def prefixedInterval(interval: String, p: ScalePrefix): String

        def build(): Seq[CanonicalizedLinearUnit] =
          prefixes.filterNot(p => _excludePrefixes.contains(p.prefix)).map { p =>
            val al = (p.prefix +: p._aliases).flatMap(ps => symbols.map(ps + _)).tail
            val pname = prefixedName(name, p)
            CanonicalizedLinearUnit(pname, toObjectName(pname), p.prefix+symbol, al,
              prefixedInterval(baseInterval, p), null, LinearUnit.this.notExact, Nil)
          }
      }

      class DefaultPrefixedUnitsBuilder extends PrefixedUnitsBuilder{
        override def prefixedName(name: String, p: ScalePrefix): String =  p.name + name
        override def prefixedInterval(interval: String, p: ScalePrefix): String = s"""$interval * r"${p.scale}""""
      }

      class LengthPoweredPrefixedUnitsBuilder(baseUnitName: String) extends PrefixedUnitsBuilder{
        private val powerName = baseUnitName.split(' ')(0)
        // square metre -> square millimetre
        override def prefixedName(name: String, p: ScalePrefix): String =  s"$powerName ${p.name}metre"
        // LengthUnitObjects.metre**2 -> LengthUnitObjects.millimetre**2
        override def prefixedInterval(interval: String, p: ScalePrefix): String = interval.replace("metre", p.name+"metre")
      }

      class TimeSquaredUnitsBuilder extends PrefixedUnitsBuilder{
        // second squared -> millisecond squared
        override def prefixedName(name: String, p: ScalePrefix): String =  p.name + name
        // TimeUnitObjects.second**2 -> TimeUnitObjects.millisecond**2
        override def prefixedInterval(interval: String, p: ScalePrefix): String = interval.replace("second", p.name+"second")
      }

      val builder = this.name match {
        case "square metre" | "cubic metre" => new LengthPoweredPrefixedUnitsBuilder(this.name)
        case "second squared" => new TimeSquaredUnitsBuilder
        case _ => new DefaultPrefixedUnitsBuilder
      }

      head +: builder.build()

    } else{
      val u = CanonicalizedLinearUnit(this.name, toObjectName(this.name), this.symbol, this._aliases, baseInterval,
                                                  null, this.notExact, this._attributes)

      val us = this._attributes.map{ a =>
        val _name = s"${u.name}(${a.name})"
        CanonicalizedLinearUnit(
          _name, toObjectName(_name), s"${u.symbol}(${a.name})", this._aliases.map(al => s"$al(${a.name})"),
          makeIntervalExpression(a.interval, a.baseUnit), null, a.notExact, Nil)
      }

      u +: us
    }
  }
}

case class Attribute(name: String, interval: String, baseUnit: String, notExact: Boolean)

case class CanonicalizedLinearUnit(name: String, objectName: String, symbol: String, aliases: Seq[String],
                                    interval: String, baseUnit: String, notExact: Boolean, attributes: Seq[Attribute]){
  require(this.attributes != null)
}

class OperationInfo(val mul: Seq[(UnitDefinitionJson, UnitDefinitionJson)], val div: Seq[(UnitDefinitionJson, UnitDefinitionJson)]){
  def ops: Seq[(UnitDefinitionJson, UnitDefinitionJson)] = Seq.concat(mul, div)
}

object OperationInfo{
  def apply(id: String, jsons: JsonResources): OperationInfo = {
    val mul = jsons.unitDefs.flatMap { ud =>
        ud.composites.products.filter(_._1 == id)
          .map(p => (jsons.searchUnitDefinition(p._2), ud))
      }

    val div = jsons.unitDefs.flatMap { ud =>
        ud.composites.quotients.filter(_._1 == id)
          .map(p => (jsons.searchUnitDefinition(p._2), ud))
      }

    new OperationInfo(mul, div)
  }
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

  protected def doGenerate(jsons: JsonResources): Unit = {
    IO.writer(this.destFile, "", utf8, append = false) { writer: io.BufferedWriter =>

      val units = this.unitCategory._units.flatMap(_.canonicalizeAndExpandScalePrefixes(jsons))
      val op = OperationInfo(this.id, jsons)

      generateGlobalImports(writer, jsons, op)
      generateQuantity(writer, jsons, op)
      generateUnitTrait(writer, op)
      generateUnitCompanionObject(writer, jsons, units)
      generateAttributes(writer, units)
      generateUnitObjects(writer, jsons, units)
      generateUnits(writer, units)
    }
  }

  protected def generateGlobalImports(writer: BufferedWriter, jsons: JsonResources, op: OperationInfo): Unit = {
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

    val ops = op.ops.flatMap(p => Seq(p._1, p._2)).map(_.id)
    writeDownImportsOfExtraUnitTypes(writer, jsons, ops, 0, "", "Unit")
    writer.write("\n")
  }

  private def writeDownImportsOfExtraUnitTypes(writer: BufferedWriter, jsons: JsonResources,
                                               types: Seq[String], indent: Int, imported: String*): Unit = {
    val sIndent = " "*indent
    types.distinct.map(jsons.searchUnitDefinition).foreach{ ud =>
      imported.filterNot(!_.contains("._") && ud.subpackage == this.subpackage)
        .map(s => s"""${sIndent}import $rootPackage.unit.${ud.subpackage}.${ud.id}$s\n""").foreach(writer.write)

      if (imported.length > 1) writer.write("\n")
    }
  }

  private def generateQuantity(writer: BufferedWriter, jsons: JsonResources, op: OperationInfo): Unit = {

    writer.write(
      s"""class $id[A: Fractional](val value: A, val unit: ${id}Unit)
         |    extends LinearQuantity[$id[A], A, ${id}Unit] {
         |
         |  override protected def newQuantity(value: A, unit: ${id}Unit): $id[A] = new $id(value, unit)
         |
         |""".stripMargin)

    op.mul.foreach(generateQuantityMultiplication(writer, _))

    op.div.foreach{ p =>
      val denoType = p._1.id
      val resultType = p._2.id
      val denoId = headToLower(denoType)
      writer.write(
        s"""  def /($denoId: $denoType[A]): $resultType[A] = new $resultType(this.value / $denoId.value, this.unit / $denoId.unit)
           |
           |""".stripMargin)
    }

    if (this.unitCategory._convertibles.exists(_.factor.contains("Constants")))
      writer.write(s"""  import $rootPackage.unit.Constants\n""")

    if (this.unitCategory._convertibles.map(_.from).exists(!_.contains('.')))
      writer.write(s"""  import ${id}UnitObjects._\n""")

    this.unitCategory._convertibles.foreach{ conv =>
      writeDownImportsOfExtraUnitTypes(writer, jsons, Seq(conv.target), 2, "")

      if (!conv.to.contains('.'))
        writeDownImportsOfExtraUnitTypes(writer, jsons, Seq(conv.target), 2, "UnitObjects._")

      val extraTypes = Seq(conv.from, conv.to).flatMap(extractUnitTypes)
      writeDownImportsOfExtraUnitTypes(writer, jsons, extraTypes, 2, "UnitObjects")

      writer.write(
        s"""
           |  def to${conv.target}: ${conv.target}[A] =
           |    new ${conv.target}(
           |      apply(${refineUnitNamesInConvertible(conv.from)}) * implicitly[Fractional[A]].fromReal(${refineNumbers(conv.factor)}),
           |      ${refineUnitNamesInConvertible(conv.to)})
           |
           |""".stripMargin)
    }

    writer.write("}\n\n")
  }

  protected def generateUnitTrait(writer: BufferedWriter, op: OperationInfo): Unit = {
    writer.write(
      s"""trait ${id}Unit extends LinearUnit[${id}Unit]{
         |
         |  override def getSIUnit: ${id}Unit = ${id}Unit.getSIUnit
         |  override def dimension: Map[DimensionSymbol, Int] = ${id}Unit.dimension
         |
         |""".stripMargin)

    op.mul.foreach(generateUnitMultiplication(writer, _))

    op.div.foreach{ p =>
      val denoType = p._1.id
      val resultType = p._2.id
      val denoId = headToLower(denoType)
      writer.write(
        s"""  def /(${denoId}Unit: ${denoType}Unit): ${resultType}Unit =
           |    new AbstractQuotientUnit[${resultType}Unit, ${id}Unit, ${denoType}Unit](${id}Unit.this, ${denoId}Unit) with ${resultType}Unit
           |
           |""".stripMargin)
    }

    writer.write("}\n\n")
  }

  protected def generateQuantityMultiplication(writer: BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    val secondType = p._1.id
    val resultType = p._2.id
    val secondId = headToLower(secondType)
    writer.write(
      s"""  def *($secondId: $secondType[A]): $resultType[A] = new $resultType(this.value * $secondId.value, this.unit * $secondId.unit)
         |
         |""".stripMargin)
  }

  private def generateUnitCompanionObject(writer: BufferedWriter, jsons: JsonResources, units: Seq[CanonicalizedLinearUnit]): Unit = {
    writer.write(s"""object ${id}Unit{\n""")

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

      writeDownImportsOfExtraUnitTypes(writer, jsons, List(first, second), 2, "Unit")

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

  protected def generateUnitMultiplication(writer: io.BufferedWriter, p: (UnitDefinitionJson, UnitDefinitionJson)): Unit = {
    val secondType = p._1.id
    val resultType = p._2.id
    val secondId = headToLower(secondType)
    writer.write(
      s"""  def *(${secondId}Unit: ${secondType}Unit): ${resultType}Unit =
         |    new AbstractProductUnit[${resultType}Unit, ${id}Unit, ${secondType}Unit](${id}Unit.this, ${secondId}Unit) with ${resultType}Unit
         |
         |""".stripMargin)
  }

  private def generateAttributes(writer: BufferedWriter, units: Seq[CanonicalizedLinearUnit]): Boolean = {
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
    writer.write("}\n\n")
    true
  }

  private def generateUnitObjects(
       writer: BufferedWriter, jsons: JsonResources, units: Seq[CanonicalizedLinearUnit]): Unit = {

    writer.write(
      s"""class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
         |  extends ${id}Unit
         |
         |""".stripMargin)

    writer.write(s"object ${id}UnitObjects{\n")

    if(units.exists(u => u.interval != null && u.interval.contains("Constants")))
      writer.write(s"""  import $rootPackage.unit.Constants\n""")

    this.unitCategory._units.map(_.baseUnit).filterNot(_ == null)
        .flatMap(extractUnitTypes).distinct.map(jsons.searchUnitDefinition).foreach{ ud =>
      if (ud.subpackage != this.subpackage)
        writer.write(s"""  import $rootPackage.unit.${ud.subpackage}.${ud.id}UnitObjects\n""")
    }

    if(id == "TimeSquared")
      writer.write(s"""  import $rootPackage.unit.basic.TimeUnitObjects\n""")

    writer.write("\n")

    //***** Unit Objects *****
    units.foreach{ u =>
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

    writer.write("}\n\n")
  }

  private def generateUnits(writer: BufferedWriter, units: Seq[CanonicalizedLinearUnit]): Unit = {

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
          |      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".squared")
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
          |      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".cubic")
          |    }
          |
          |  def *(lengthUnit: LengthUnit): AreaUnit =
          |    if(this == lengthUnit)
          |      this.squared
          |    else
          |      new AbstractProductUnit[AreaUnit, LengthUnit, LengthUnit](LengthUnit.this, lengthUnit) with AreaUnit
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
           |      override def aliases: Seq[String] = TimeUnit.this.symbols.map(_+".squared")
           |    }
           |
           |  def *(timeUnit: TimeUnit): TimeSquaredUnit =
           |    if(this == timeUnit)
           |      this.squared
           |    else
           |      new AbstractProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit](TimeUnit.this, timeUnit) with TimeSquaredUnit
           |""".stripMargin)
    }else{
      super.generateUnitMultiplication(writer, p)
    }
}