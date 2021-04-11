import java.io.{File, BufferedWriter => BW}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class LinearUnitCategory(description: String,
                              SIUnit: String,
                              dimension: Dimension,
                              convertibles: Array[Convertible],
                              units: Array[LinearUnit],
                              operations: Array[Operation],
                              attributes: Array[Attribute],
                              use: Use) extends UnitCategory[LinearUnit]{

  import GenerationUtil._
  def _operations: Seq[Operation] = toSeq(this.operations)
  def _attributes: Seq[Attribute] = toSeq(this.attributes)
}

case class Operation(operation: String, argument: String, result: String, reverse: Boolean)
case class Attribute(name: String, parents: Array[String]){
  require(parents.nonEmpty)
}

case class LinearUnit(name: String,
                      attribute: String,
                      symbol: String,
                      aliases: Array[String],
                      interval: String,
                      baseUnit: String,
                      attributes: Array[String],
                      notExact: Boolean,
                      description: String) extends UnitInfo{

  import GenerationUtil._

  lazy val objectName: String =
    if (this.attribute != null) toObjectName(s"$name($attribute)")
    else toObjectName(this.name)

  def _attributes: Seq[String] = toSeq(this.attributes)

  lazy val intervalExpression: String = 
    if (this.baseUnit == null) {
      if (this.interval == null) "1"
      else refineNumbers(this.interval)

    } else {
      val baseUnitInterval = regexUnitName.replaceAllIn(this.baseUnit, m => {
        val uType = if (m.group(1) != null) m.group(1) + "UnitObjects." else ""
        val uName = escape(m.group(2))
        s"$uType$uName.interval"
      }).replaceAll("""\^""", "**")  // Length.metre^2 => Length.metre.interval**2

      if (this.interval == null) baseUnitInterval
      else s"""${refineNumbers(this.interval)} * $baseUnitInterval"""
    }
}

class LinearUnitdefJson(jsonFile: File, subpackage: String)
    extends UnitdefJsonAdapter[LinearUnitCategory, LinearUnit](
      jsonFile, subpackage, UnitType.Linear) {

  import GenerationUtil._

  val unitCategoryType: Class[_ >: LinearUnitCategory] = new TypeToken[LinearUnitCategory]() {}.getRawType

  val unitCategory: LinearUnitCategory = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitCategoryType).asInstanceOf[LinearUnitCategory]
  }

  override def needSpireImplicits: Boolean = 
    super.needSpireImplicits || this.unitCategory._operations.nonEmpty

  override protected def parentQuantityDeclaration: String = s"""LinearQuantity[$id[A], A, ${id}Unit]"""

  override protected def generateQuantityOperations(writer: BW): Unit = {
    val ops = this.unitCategory._operations

    writer.write(
      s"""  override protected def newQuantity(value: A, unit: ${id}Unit): $id[A] = new $id(value, unit)\n""")

    ops.foreach(generateQuantityOperation(writer, _))
  }

  protected def generateQuantityOperation(writer: BW, ope: Operation): Unit = {
    val op = ope.operation
    val argType = ope.argument
    val resultType = ope.result
    val arg = headToLower(argType)
    //  like 'def /(time: Time[A]): Velocity[A] = new Velocity(this.value / time.value, this.unit / time.unit)
    val resultUnit = if (!ope.reverse) s"this.unit $op $arg.unit" else s"$arg.unit $op this.unit"
    writer.write(
      s"""
         |  def $op($arg: $argType[A]): $resultType[A] = new $resultType(this.value $op $arg.value, $resultUnit)
         |""".stripMargin)
  }

  override protected def generateUnitOperations(writer: BW): Unit =
    this.unitCategory._operations.foreach(generateUnitOperation(writer, _))

  protected def generateUnitOperation(writer: BW, ope: Operation): Unit =
    if (!ope.reverse){
      val op = ope.operation
      val name = if (op == "*") "Product" else "Quotient"
      val argType = ope.argument
      val resultType = ope.result
      val arg = headToLower(argType)
      writer.write(
        s"""
           |  def $op(${arg}Unit: ${argType}Unit): ${resultType}Unit =
           |    new ${name}Unit[${resultType}Unit, ${id}Unit, ${argType}Unit](${id}Unit.this, ${arg}Unit) with ${resultType}Unit
           |""".stripMargin)
    }

  override protected def generateImplsOfUnitTrait(writer: BW): Unit = {
    writer.write(
      s"""
         |
         |/** For no aliase or user defined units */
         |class Simple${id}Unit(val name: String, val symbol: String, val interval: Real) extends ${id}Unit {
         |  override def aliases: Seq[String] = Nil
         |}
         |
         |/** For units which has aliases */
         |class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
         |  extends ${id}Unit
         |  """.stripMargin)
  }

  override protected def generateUnitCaseObject(writer: BW, unit: LinearUnit): Unit = {
    val notExact = if (!unit.notExact) "" else " with NotExact"
    val desc = if (unit.description != null) {
      s""" with Description {
         |    def description: String = "${unit.description}"
         |  }""".stripMargin
    } else ""

    if (unit._aliases.isEmpty){
      // final case object metre extends SimpleLengthUnit("metre", "m", Nil, r"1")
      writer.write(
        s"""  final case object ${unit.objectName} extends Simple${id}Unit""" +
          s"""("${unit.name}", "${unit.symbol}", ${unit.intervalExpression})$notExact$desc\n""")
    }else{
      // final case object metre extends DefaultLengthUnit("micrometre", "μm", Seq("mcm"), r"1e-6")
      writer.write(
        s"""  final case object ${unit.objectName} extends Default${id}Unit""" +
          s"""("${unit.name}", "${unit.symbol}", ${unit.aliasesStr}, ${unit.intervalExpression})$notExact$desc\n""")
    }
  }

  // overridden by Length and LengthPowered unitdefs
  protected def attributeContainerID: String = this.id + "Units"

  override protected def generateUnits(writer: BW): Unit = {
    generateAttributeTraits(writer)

    val units = this.unitCategory._units

    writer.write(
      s"""
         |object ${id}Units{
         |""".stripMargin)

    generateAttributeObjects(writer)
    writer.write("\n")

    units.filter(_.attribute == null).foreach { u =>
      val sym = escape(u.symbol)
      val rType = getReturnedTypeOfUnits(u)

      // /** kilometre */
      // def km: LengthUnit = LengthUnitObjects.killometre
      writer.write(
        s"""  /** ${u.name} */
            |  def $sym: $rType = ${id}UnitObjects.${u.objectName}
            |""".stripMargin)

      if (u._attributes.nonEmpty) {
        // /** ounce(avoirdupois)<br/>ounce(troy) */
        // def oz(a: ounceAttribute): MassUnit = a match {
        //   case MassUnits.avoirdupois => MassUnitObjects.`ounce(avoirdupois)`
        //   case MassUnits.troy => MassUnitObjects.`ounce(troy)`
        // }
        val doc = u._attributes.map(a => s"${u.objectName}($a)").mkString("<br/>")
        writer.write(
          s"""  /** $doc */
             |  def $sym(a: ${u.objectName}Attribute): $rType = a match {
             |""".stripMargin)
        u._attributes.foreach { a =>
          writer.write(s"""    case $attributeContainerID.$a => ${id}UnitObjects.`${u.objectName}($a)`\n""")
        }
        writer.write("  }\n")
      }

      u._aliases.foreach { al =>
        val als = escape(al)
        writer.write(
          s"""  /** ${u.name} */
               |  def $als: $rType = ${id}UnitObjects.${u.objectName}
               |""".stripMargin)
        if (u._attributes.nonEmpty) {
          // def nmi(a: nautical_mileAttribute): LengthUnit = NM(a)
          val doc = u._attributes.map(a => s"  ${u.objectName}($a)").mkString("<br/>")
          writer.write(
            s"""  /** $doc */
               |  def $als(a: ${u.objectName}Attribute): $rType = $sym(a)
               |""".stripMargin)
        }
      }
    }

    writer.write("}")
  }

  protected def generateAttributeTraits(writer: BW): Unit =
    this.unitCategory._attributes.flatMap(a => a.parents).distinct.foreach{ u =>
      writer.write(s"""sealed trait ${toObjectName(u)}Attribute\n""")
    }

  /* This method is overridden by Length and LengthPowered UnitdefsJson */
  protected def generateAttributeObjects(writer: BW): Unit =
    this.unitCategory._attributes.foreach{ att =>
      val traits = att.parents.map(toObjectName(_) + "Attribute").mkString(" with ")
      writer.write(s"""  final object ${att.name} extends $traits\n""")
    }

  /* This method is overridden by LengthPowered and TimeSquared UnitdefsJson */
  protected def getReturnedTypeOfUnits(u: LinearUnit): String = s"${id}Unit"
}

class LengthUnitdefJson(jsonFile: File, subpackage: String)
    extends LinearUnitdefJson(jsonFile, subpackage){

  override protected def attributeContainerID: String = "MetricAttributes"

  override protected def generateQuantityOperation(writer: BW, ope: Operation): Unit = {
    super.generateQuantityOperation(writer, ope)
    if (ope.argument == "Length")
      writer.write(
        s"""  def squared: Area[A] = this * this
           |  def cubic: Volume[A] = this * this * this
           |
           |""".stripMargin)
  }

  override protected def getAdditionalTraitsOfUnit: Seq[String] = Seq("LengthUnitCanSquare", "LengthUnitCanCubic")

  override protected def generateUnitOperation(writer: BW, ope: Operation): Unit =
    if (ope.argument != "Length")
      super.generateUnitOperation(writer, ope)

  override protected def generateAttributeTraits(writer: BW): Unit = ()
  override protected def generateAttributeObjects(writer: BW): Unit = ()
}

class LengthPoweredUnitdefJson(jsonFile: File, subpackage: String, unitName: String)
    extends LinearUnitdefJson(jsonFile, subpackage){

  import GenerationUtil._

  private val powerPrefix = if (unitName == "Area") "square" else "cubic"

  override protected def attributeContainerID: String = "MetricAttributes"

  override protected def generateAttributeTraits(writer: BW): Unit = ()
  override protected def generateAttributeObjects(writer: BW): Unit = ()

  override protected def generateUnitCaseObject(writer: BW, unit: LinearUnit): Unit = {
    if (unit.name.startsWith(powerPrefix)) {
      val baseUnit = refineUnitNamesInPoweredBaseUnit(unit.baseUnit)  // Length.metre^2 => LengthUnitObjects.metre
      writer.write(
        s"""  final case object ${unit.objectName} extends """ +
          s"""LengthPowered${unitName}Unit(LengthUnitObjects.$baseUnit, ${unit.aliasesStr})\n""")
    } else {
      super.generateUnitCaseObject(writer, unit)
    }
  }

  override protected def getReturnedTypeOfUnits(u: LinearUnit): String =
    if (u.name.startsWith(powerPrefix)) s"LengthPowered${unitName}Unit"
    else super.getReturnedTypeOfUnits(u)
}

class TimeUnitdefJson(jsonFile: File, subpackage: String)
    extends LinearUnitdefJson(jsonFile, subpackage){

  override protected def getAdditionalTraitsOfUnit: Seq[String] = Seq("TimeUnitCanSquare")

  override protected def generateQuantityOperation(writer: BW, ope: Operation): Unit = {
    super.generateQuantityOperation(writer, ope)
    if (ope.argument == "Time")
      writer.write(s"""  def squared: TimeSquared[A] = this * this\n""")
  }

  override protected def generateUnitOperation(writer: BW, ope: Operation): Unit =
    if (ope.argument != "Time")
      super.generateUnitOperation(writer, ope)
}

class TimeSquaredUnitdefJson(jsonFile: File, subpackage: String)
  extends LinearUnitdefJson(jsonFile, subpackage){

  import GenerationUtil._

  override protected def generateUnitCaseObject(writer: BW, unit: LinearUnit): Unit = {
    val baseUnit = refineUnitNamesInPoweredBaseUnit(unit.baseUnit)  // Time.second^2 => TimeUnitObjects.second
    writer.write(
      s"""  final case object ${unit.objectName} extends """ +
        s"""TimePoweredTimeSquaredUnit(TimeUnitObjects.$baseUnit, ${unit.aliasesStr})\n""")
  }

  override protected def getReturnedTypeOfUnits(u: LinearUnit): String =
    if (u.name.endsWith("squared")) "TimePoweredTimeSquaredUnit"
    else super.getReturnedTypeOfUnits(u)
}