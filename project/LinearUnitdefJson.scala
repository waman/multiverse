import java.io.{File, BufferedWriter => BW}
import play.api.libs.json.{Json, Reads}

case class LinearUnitdef(description: Option[String],
                         si_unit: String,
                         dimension: Dimension,
                         convertibles: Option[Seq[Convertible]],
                         units: Option[Seq[LinearUnit]],
                         operations: Option[Seq[Operation]],
                         attributes: Option[Seq[Attribute]],
                         use: Option[Use]) extends Unitdef[LinearUnit]

case class Operation(operation: String, argument: String, result: String, reverse: Option[Boolean])
case class Attribute(name: String, parents: Seq[String])

case class LinearUnit(name: String,
                      attribute: Option[String],
                      symbol: String,
                      aliases: Option[Seq[String]],
                      interval: Option[String],
                      base_unit: Option[String],
                      attributes: Option[Seq[String]],
                      not_exact: Option[Boolean],
                      description: Option[String]) extends UnitInfo{

  import GenerationUtil._

  lazy val objectName: String = this.attribute match {
    case Some(att) => toSnakeCase(s"$name($att)")
    case _ => toSnakeCase(this.name)
  }

  lazy val intervalExpression: String = 
    this.base_unit match {
      case Some(_baseUnit) =>
        val baseUnitInterval = regexUnitName.replaceAllIn(_baseUnit, m => {
          val uType = if (m.group(1) != null) m.group(1) + "UnitObjects." else ""
          val uName = escape(m.group(2))
          s"$uType$uName.interval"
        }).replaceAll("""\^""", "**")  // Length.metre^2 => Length.metre.interval**2

        this.interval match {
          case Some(_interval) => s"""${refineFactor(_interval)} * $baseUnitInterval"""
          case _ => baseUnitInterval
        }
      case _ =>
        this.interval match {
          case Some(i) => refineFactor(i) 
          case _ => "1"
        }
    }
}

class LinearUnitdefJson(jsonFile: File, subpackage: String)
    extends UnitdefJsonAdapter[LinearUnitdef, LinearUnit](
      jsonFile, subpackage, UnitType.Linear) {

  import GenerationUtil._

  implicit val linearUnitReads: Reads[LinearUnit] = Json.reads[LinearUnit]
  implicit val operationReads: Reads[Operation] = Json.reads[Operation]
  implicit val attributeUnitReads: Reads[Attribute] = Json.reads[Attribute]
  implicit val linearUnitdefReads: Reads[LinearUnitdef] = Json.reads[LinearUnitdef]
  val unitdef: LinearUnitdef = readJson(jsonFile, _.validate[LinearUnitdef])

  override def needSpireImplicits: Boolean = 
    super.needSpireImplicits || this.unitdef.operations.nonEmpty

  override protected def parentQuantityDeclaration: String = s"""LinearQuantity[$id[A], A, ${id}Unit]"""

  override protected def generateQuantityOperations(writer: BW): Unit = {
    writer.write(
      s"""  override protected def newQuantity(value: A, unit: ${id}Unit): $id[A] = new $id(value, unit)\n""")

    this.unitdef.operations.foreach{ ops =>
      ops.foreach(generateQuantityOperation(writer, _))
    }
  }

  protected def generateQuantityOperation(writer: BW, ope: Operation): Unit = {
    val op = ope.operation
    val argType = ope.argument
    val resultType = ope.result
    val arg = headToLower(argType)

    val resultUnit = ope.reverse match {
      case Some(true) => s"$arg.unit $op this.unit"
      case _ => s"this.unit $op $arg.unit"
    }

    //  like 'def /(time: Time[A]): Velocity[A] = new Velocity(this.value / time.value, this.unit / time.unit)
    writer.write(
      s"""
         |  def $op($arg: $argType[A]): $resultType[A] = new $resultType(this.value $op $arg.value, $resultUnit)
         |""".stripMargin)
  }

  override protected def generateUnitOperations(writer: BW): Unit =
    this.unitdef.operations.foreach{ ops =>
      ops.foreach(generateUnitOperation(writer, _))
    }


  protected def generateUnitOperation(writer: BW, ope: Operation): Unit = {
    ope.reverse match {
      case Some(true) =>  // do nothing
      case  _ =>
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
  }

  override protected def generateImplementationsOfUnitTrait(writer: BW): Unit = {
    writer.write(
      s"""
         |
         |/** For no alias or user defined units */
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
    val notExact = unit.not_exact match {
      case Some(true) => " with NotExact"
      case _ => ""
    }

    val desc = unit.description match {
      case Some(_description) =>
        s""" with Description {
           |    def description: String = "${_description}"
           |  }""".stripMargin
      case _ => ""
    }

    unit.aliases match {
      case Some(_) =>
        // final case object metre extends DefaultLengthUnit("micrometre", "μm", Seq("mcm"), r"1e-6")
        writer.write(
          s"""  final case object ${unit.objectName} extends Default${id}Unit""" +
            s"""("${unit.name}", "${unit.symbol}", ${unit.aliasesStr}, ${unit.intervalExpression})$notExact$desc\n""")
      case _ =>
        // final case object metre extends SimpleLengthUnit("metre", "m", Nil, r"1")
        writer.write(
          s"""  final case object ${unit.objectName} extends Simple${id}Unit""" +
            s"""("${unit.name}", "${unit.symbol}", ${unit.intervalExpression})$notExact$desc\n""")
    }
  }

  // overridden by Length and LengthPowered unitdefs
  protected def attributeContainerID: String = this.id + "Units"

  override protected def generateUnits(writer: BW): Unit = {
    generateAttributeTraits(writer)

    this.unitdef.units.foreach{ units =>
      writer.write(
        s"""
           |object ${id}Units{
           |""".stripMargin)

      generateAttributeObjects(writer)
      writer.write("\n")

      units.filter(_.attribute.isEmpty).foreach { u =>
        val sym = escape(u.symbol)
        val rType = getReturnedTypeOfUnits(u)

        // /** kilometre */
        // def km: LengthUnit = LengthUnitObjects.killometre
        writer.write(
          s"""  /** ${u.name} */
             |  def $sym: $rType = ${id}UnitObjects.${u.objectName}
             |""".stripMargin)

        u.attributes.foreach{ attributes =>
          // /** ounce(avoirdupois)<br/>ounce(troy) */
          // def oz(a: ounceAttribute): MassUnit = a match {
          //   case MassUnits.avoirdupois => MassUnitObjects.`ounce(avoirdupois)`
          //   case MassUnits.troy => MassUnitObjects.`ounce(troy)`
          // }
          val doc = attributes.map(a => s"${u.objectName}($a)").mkString("<br/>")
          writer.write(
            s"""  /** $doc */
               |  def $sym(a: ${u.objectName}Attribute): $rType = a match {
               |""".stripMargin)
          attributes.foreach { a =>
            writer.write(s"""    case $attributeContainerID.$a => ${id}UnitObjects.`${u.objectName}($a)`\n""")
          }
          writer.write("  }\n")
        }

        u.aliases.foreach{ aliases =>
          aliases.foreach { al =>
            val als = escape(al)
            writer.write(
              s"""  /** ${u.name} */
                 |  def $als: $rType = ${id}UnitObjects.${u.objectName}
                 |""".stripMargin)
            u.attributes.foreach{ attributes =>
              // def nmi(a: nautical_mileAttribute): LengthUnit = NM(a)
              val doc = attributes.map(a => s"  ${u.objectName}($a)").mkString("<br/>")
              writer.write(
                s"""  /** $doc */
                   |  def $als(a: ${u.objectName}Attribute): $rType = $sym(a)
                   |""".stripMargin)
            }
          }
        }
      }
      writer.write("}")
    }
  }

  protected def generateAttributeTraits(writer: BW): Unit =
    this.unitdef.attributes.foreach{ attributes =>
      attributes.flatMap(a => a.parents).distinct.foreach{ u =>
        writer.write(s"""sealed trait ${toSnakeCase(u)}Attribute\n""")
      }
    }

  /* This method is overridden by Length and LengthPowered UnitdefsJson */
  protected def generateAttributeObjects(writer: BW): Unit = {
    this.unitdef.attributes.foreach{ attributes =>
      attributes.foreach{ att =>
        val traits = att.parents.map(toSnakeCase(_) + "Attribute").mkString(" with ")
        writer.write(s"""  final object ${att.name} extends $traits\n""")
      }
    }
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
      val baseUnit = refineUnitNamesInPoweredBaseUnit(unit.base_unit.get)  // Length.metre^2 => LengthUnitObjects.metre
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
    val baseUnit = refineUnitNamesInPoweredBaseUnit(unit.base_unit.get)  // Time.second^2 => TimeUnitObjects.second
    writer.write(
      s"""  final case object ${unit.objectName} extends """ +
        s"""TimePoweredTimeSquaredUnit(TimeUnitObjects.$baseUnit, ${unit.aliasesStr})\n""")
  }

  override protected def getReturnedTypeOfUnits(u: LinearUnit): String =
    if (u.name.endsWith("squared")) "TimePoweredTimeSquaredUnit"
    else super.getReturnedTypeOfUnits(u)
}