import java.nio.file.Path

import com.google.gson._
import org.waman.gluino.io.FileType
import org.waman.gluino.nio.GluinoPath

import scala.collection.JavaConversions._

object MultiverseSourceGenerator extends GluinoPath{

  val parser = new JsonParser

  case class Scale(name: String, prefix: String, scale: String)
  case class NameSymbol(name: String, symbol: String)

  val scalePrefixes: Seq[Scale] = Seq(
    Scale("Yocto", "y", "1e-24"),
    Scale("Zepto", "z", "1e-21"),
    Scale("Atto" , "a", "1e-18"),
    Scale("Femto", "f", "1e-15"),
    Scale("Pico" , "p", "1e-12"),
    Scale("Nano" , "n", "1e-9"),
    Scale("Micro", "μ", "1e-6"),
    Scale("Milli", "m", "1e-3"),
    Scale("Centi", "c", "1e-2"),
    Scale("Deci" , "d", "1e-1"),
    Scale(""     , "" , "1"),
    Scale("Deca" , "da", "1e1"),
    Scale("Hecto", "h", "1e2"),
    Scale("Kilo" , "k", "1e3"),
    Scale("Mega" , "M", "1e6"),
    Scale("Giga" , "G", "1e9"),
    Scale("Tera" , "T", "1e12"),
    Scale("Peta" , "P", "1e15"),
    Scale("Exa"  , "E", "1e18"),
    Scale("Zetta", "Z", "1e21"),
    Scale("Yotta", "Y", "1e24")
  )

  def generate(rsrc: Path, srcManaged: Path): List[Path] = {
    srcManaged.createDirectories()

    val (contexts, contextLists) = generateContexts(rsrc, srcManaged)
    // 2nd Map("org.waman.multiverse.metric.MetricContext" -> List(NameSymbol("UnitedStates", "US"), NameSymbol("Imperial", "imp"), ...)

    val unitSystem = generateUnitSystem(rsrc, srcManaged, contextLists)

    val unitTraits = generateUnitTraits(rsrc, srcManaged, contextLists)

    contexts ::: unitSystem :: unitTraits
  }

  def generateContexts(rsrc: Path, srcManaged: Path): (List[Path], Map[String, List[NameSymbol]]) = {

    val seq = rsrc.filesMatchRecurse(FileType.Files, fileExtensionFilter("Context.json")).map { json =>
      val id = json.getFileName.toString.replaceAll("\\.json", "")
      val rel = rsrc.relativize(json.getParent)
      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")
      val generated = srcManaged / rel / (id + ".scala")

      val nsList: List[NameSymbol] = json.withReader(parser.parse(_))
        .getAsJsonArray.toList
        .map(_.getAsJsonObject)
        .map{ obj => NameSymbol(getString(obj, "name"), getString(obj, "symbol"))}

      if(!generated.exists || generated.isOlderThan(json) ) {
        if (generated.exists) generated.delete()
        generated.getParent.createDirectories()

        generateContext(id, packageName, generated, nsList)
        println("[GENERATE] " + generated)
      }

      (generated, packageName + "." + id, nsList)
      // (generated, "org.waman.multiverse.metric.MetricContext", List(NameSymbol("UnitedStates", "US"), ...))
    }

    (seq.map(_._1).toList, Map(seq.map(t => (t._2, t._3)):_*))
  }

  def generateContext(id: String, packageName: String, generated: Path, contextList: List[NameSymbol]): Unit = {
    generated.withWriter{ writer =>
      // Context trait
      writer <<
        s"""package $packageName
           |
           |import org.waman.multiverse.ConstantsDefined
           |
           |sealed abstract class $id(val name: String, val symbol: String)""".stripMargin

      // Context object
      writer <<
        s"""
           |
           |object $id extends ConstantsDefined[$id]{
           |""".stripMargin

      contextList.foreach { ns =>
        writer <<
          // case object UnitedStates extends Context("UnitedStates", "US")
          s"""
             |  case object ${ns.name} extends $id("${ns.name}", "${ns.symbol}")""".stripMargin
      }

      writer <<
        s"""
           |
           |  lazy val values = Seq(${contextList.map(_.name).mkString(", ")})
           |}""".stripMargin

      // ContextDefined trait
      writer <<
        s"""
           |
           |trait ${id}Defined{
           |  import $id._
           |""".stripMargin

      contextList.foreach{ ns =>
        writer <<
          // val US = Context.UnitedStates
          s"""
             |  val ${ns.symbol} = ${ns.name}""".stripMargin
      }

      writer <<
        s"""
           |}""".stripMargin
    }
  }

  def jsonPropertyFilesOfUnits(rsrc: Path): List[Path] =
    rsrc.filesMatchRecurse(FileType.Files, fileExtensionFilter("Unit.json")).toList

  def generateUnitSystem(rsrc: Path, srcManaged: Path, contextLists: Map[String, List[NameSymbol]]): Path = {
    val generated = srcManaged / "org/waman/multiverse/UnitSystem.scala"

    if(generated.exists && jsonPropertyFilesOfUnits(rsrc).forall(generated.isNewerThan))
      return generated

    if(generated.exists)generated.delete()

    val unitSystemInfo = jsonPropertyFilesOfUnits(rsrc).map { json: Path =>
      val rel = rsrc.relativize(json.getParent)
      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")
      val className = json.getFileName.toString.replace("Unit.json", "")

      val jsonProp = json.withReader(parser.parse(_)).getAsJsonObject
      if(jsonProp.has("constants"))
        List(s"$packageName.$className", s"$packageName.Predefined${className}Unit")
      else
        List(s"$packageName.$className")
    }

    generated.withWriter{ writer =>
      writer <<
        s"""package org.waman.multiverse
           |
           |trait UnitSystem extends UnitSystemImplicits""".stripMargin

      contextLists.foreach{ case (contextClass, _) =>
        writer <<
          s"""
             |  with ${contextClass}Defined""".stripMargin
      }

      unitSystemInfo.flatMap{
        case a::b::_ => List(b)
        case _ =>  Nil
      }.foreach{ p =>
        writer <<
          s"""
             |  with $p""".stripMargin
      }

      writer <<
        s"""
           |
           |object UnitSystem extends UnitSystem{
           |
           |  lazy val supportedQuantities: Set[Class[_]] = Set(
           |    """.stripMargin

      writer << unitSystemInfo.map(_.head).map("classOf[" + _ + "[_]]").mkString(
        s""",
           |    """.stripMargin)

      writer <<
        s"""
           |  )
           |}""".stripMargin
    }
    println("[GENERATE] " + generated)
    generated
  }

  def generateUnitTraits(rsrc: Path, srcManaged: Path, contextLists: Map[String, List[NameSymbol]]): List[Path] = {
    jsonPropertyFilesOfUnits(rsrc).map{ json: Path =>
      val rel = rsrc.relativize(json.getParent)
      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")

      val destDir = srcManaged / rel
      val className = json.getFileName.toString.replace("Unit.json", "")  // Length (not contain "Unit")
      val fileName = if(className == "Temperature") "TemperaturePostfixOps.scala"
                     else className + "Unit.scala"
      val generated = destDir / fileName

      if(!generated.exists || generated.isOlderThan(json)){
        if(generated.exists) generated.delete()
        destDir.createDirectories()
        generated.createFile()

        val jsonProp: JsonObject = json.withReader(parser.parse(_)).getAsJsonObject
        generateUnitTrait(generated, packageName, className, jsonProp, contextLists)
        println("[GENERATE] " + generated)
      }
      generated
    }
  }

  def generateUnitTrait(dest: Path, packageName: String, className: String,
                        json: JsonObject, contextLists: Map[String, List[NameSymbol]]): Unit = {
    val id = headToLower(className)  // length
    val units: List[UnitConstant] = getUnitConstants(json)
    val (contextful, contextless) = getUnitNameSymbolTuples(units)
    // contextful: List(NameSymbol("Foot", "ft(US)"), ...)
    // contextless: List(NameSymbol("Metre", "m"), NameSymbol("Foot", "ft"), ...)

    dest.withWriter { writer =>

      writer <<
        s"""package $packageName
           |
           |import spire.math.Real
           |import spire.implicits._
           |import org.waman.multiverse._
           |""".stripMargin

      getChildren(json, "imports").map(_.getAsString).foreach{ i =>
        writer <<
          s"""
             |import $i""".stripMargin
      }

      if(className != "Temperature"){
        //***** Unit trait  *****
        val baseUnitAccessor = getString(json, "baseUnitAccessor")
        val baseUnit = getString(json, "baseUnit") match {
          case s if s.contains("*") || s. contains("/") => s
          case s => packageName + "." + className + "Unit." + s
        }

        val muls = getChildren(json, "multiplicative").map(_.getAsJsonArray.toList.map(_.getAsString))
          .map(p => (p.head, p(1)))

        val divs  = getChildren(json, "divisible").map(_.getAsJsonArray.toList.map(_.getAsString))
          .map(q => (q.head, q(1)))

        writer <<
          s"""
             |
             |sealed trait ${className}Unit extends PhysicalUnit[${className}Unit]""".stripMargin

        muls.foreach{ case (by, result) =>
          writer <<
            s"""
               |  with MultiplicativeBy$by[$result]""".stripMargin
        }

        divs.foreach{ case (by, result) =>
          writer <<
            s"""
                |  with DivisibleBy$by[$result]""".stripMargin
        }

        if(json.has("square")){
          val square = json.getAsJsonPrimitive("square").getAsString
          writer <<
            s"""
               |  with CanSquare[$square]""".stripMargin
        }

        if(json.has("cubic")){
          val cubic = json.getAsJsonPrimitive("cubic").getAsString
          writer <<
            s"""
               |  with CanCubic[$cubic]""".stripMargin
        }

        writer <<
          s"""{
             |
             |  def $baseUnitAccessor: Real
             |
             |  override def baseUnit = $baseUnit
             |  override def valueInBaseUnit = $baseUnitAccessor""".stripMargin

        muls.foreach{ case (arg, result) =>
          writer <<
            s"""
               |
               |  override def *(unit: $arg) = $result(this, unit)""".stripMargin
        }

        divs.foreach{ case (arg, result) =>
          writer <<
            s"""
               |
               |  override def /(unit: $arg) = $result(this, unit)""".stripMargin
        }

        if(json.has("square")){
          val square = json.getAsJsonPrimitive("square").getAsString
          writer <<
            s"""
               |
               |  override def square: $square = this * this""".stripMargin
        }

        if(json.has("cubic")){
          val cubic = json.getAsJsonPrimitive("cubic").getAsString
          writer <<
            s"""
               |
               |  override def cubic: $cubic = this * this * this""".stripMargin
        }

        writer <<
          s"""
             |}""".stripMargin

        //***** Unit object *****
        writer <<
          s"""
             |
             |object ${className}Unit extends ConstantsDefined[${className}Unit]{
             |
             |  // intrinsic
             |  private[${className}Unit]
             |  class Intrinsic${className}Unit(name: String, val symbols: Seq[String], val $baseUnitAccessor: Real)
             |      extends ${className}Unit{
             |
             |    def this(name: String, symbols: Seq[String], unit: ${className}Unit) =
             |      this(name, symbols, unit.$baseUnitAccessor)
             |
             |    def this(name: String, symbols: Seq[String], factor: Real, unit: ${className}Unit) =
             |      this(name, symbols, factor * unit.$baseUnitAccessor)
             |  }
             |
             |""".stripMargin

        units.foreach{ unit =>
          //  case object Metre extends IntrinsicLengthUnit("Metre", Seq("m"), r"1")
          writer <<
            s"""
               |  case object ${unit.name} extends Intrinsic${className}Unit""".stripMargin
          writer << s"""("${unit.name}", Seq(${unit.symbols.map(quote).mkString(", ")}), ${unit.args})"""
          if(unit.isNotExact) writer << " with NotExact"
          if(unit.mixed.length > 0){
            writer <<
              s"""
                 |    ${unit.mixed}""".stripMargin  // for Degree
          }
        }

        writer <<
          //  Seq(..., MilliMetre, CentiMetre, Metre, ...)
          s"""
             |
             |  override lazy val values = Seq(${units.map(_.name).mkString(", ")})""".stripMargin

        // product unit
        if(json.has("product")) {
          getChildren(json, "product").map(_.getAsJsonArray.toList.map(_.getAsJsonPrimitive.getAsString))
              .foreach{ case first::second::_ =>
            val productUnit = "Product" + first.replaceAll("Unit", "") +
                                "Dot" + second.replaceAll("Unit", "") + "Unit"
            writer <<
              s"""
                 |
                 |  // $first * $second -> $className
                 |  private[${className}Unit]
                 |  class $productUnit(val firstUnit: $first, val secondUnit: $second)
                 |      extends ${className}Unit with ProductUnit[${className}Unit, $first, $second]{
                 |
                 |    override lazy val $baseUnitAccessor: Real =
                 |      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
                 |  }
                 |
                 |  def apply(unit1: $first, unit2: $second): ${className}Unit =
                 |    new $productUnit(unit1, unit2)""".stripMargin

              case _ =>
          }
        }

        // quotient unit
        if(json.has("quotient")) {
          getChildren(json, "quotient").map(_.getAsJsonArray.toList.map(_.getAsJsonPrimitive.getAsString))
              .foreach { case numerator :: denominator :: _ =>
            val quotientUnit = "Quotient" + numerator.replaceAll("Unit", "") +
                               "Per" + denominator.replaceAll("Unit", "") + "Unit"
            writer <<
              s"""
                 |
                 |  // $numerator / $denominator -> $className
                 |  private[${className}Unit]
                 |  class $quotientUnit(val numeratorUnit: $numerator, val denominatorUnit: $denominator)
                 |      extends ${className}Unit with QuotientUnit[${className}Unit, $numerator, $denominator]{
                 |
                 |    override lazy val $baseUnitAccessor: Real =
                 |      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
                 |  }
                 |
                 |  def apply(nUnit: $numerator, dUnit: $denominator): ${className}Unit =
                 |    new $quotientUnit(nUnit, dUnit)""".stripMargin

              case _ =>
          }
        }

        writer <<
          s"""
             |}""".stripMargin
      }

      //***** MultiplicativeBy and DivisibleBy trait *****
      writer <<
        s"""
           |
           |trait MultiplicativeBy${className}Unit[R]{
           |  def *(unit: ${className}Unit): R
           |}
           |
           |trait DivisibleBy${className}Unit[R]{
           |  def /(unit: ${className}Unit): R
           |}""".stripMargin

      if(units.nonEmpty) {
        //***** PostfixOps trait *****
        writer <<
          s"""
             |
             |trait ${className}PostfixOps[A]{
             |  import ${className}Unit._
             |
             |  protected def ${id}PostfixOps(unit: ${className}Unit): A
             |
             |""".stripMargin

        contextless.foreach { ns =>
          writer <<
            // def m: A = lengthPostfixOps(LengthUnit.Metre)
            s"""               |
               |  def ${ns.symbol} : A = ${id}PostfixOps(${ns.name})""".stripMargin
        }

        val contextFullClassName = if(json.has("context"))getString(json, "context") else ""
        val contextClassName =
          if(contextFullClassName.length > 0)
            contextFullClassName.substring(contextFullClassName.lastIndexOf(".") + 1)
          else ""

        if (contextful.nonEmpty) {
          writer <<
            s"""
               |
               |  import ${className}PostfixOps._
               |  import $contextFullClassName
               |  import $contextClassName._
               |""".stripMargin

          contextful.map(_.symbol.split("\\(")(0)).distinct.foreach { sym =>
            writer <<
              // def ft(c: MetricContext): A = lengthPostfixOps(_ft(c))
              s"""
                 |  def $sym(c: $contextClassName): A = ${id}PostfixOps(_$sym(c))""".stripMargin
          }
        }

        writer <<
          s"""
             |}""".stripMargin

        //***** PostfixOps object *****
        if (contextful.nonEmpty) {
          val contextList = contextLists(contextFullClassName)
          writer <<
            s"""
               |
               |object ${className}PostfixOps{
               |  import ${className}Unit._
               |  import $contextFullClassName
               |  import $contextClassName._
               |""".stripMargin

          groupingContextList(contextful, contextList).foreach { case (symbol, cs) =>
            // (
            //   "ft",
            //   List(("UnitedStates", "Foot_US_Survey"), ...)
            // )

            writer <<
              s"""
                 |
                 |  lazy val _$symbol : PartialFunction[$contextClassName, ${className}Unit] = {""".stripMargin

            cs.foreach { case (c, u) =>
              writer <<
                // case US => Foot_US_Survey
                s"""
                   |    case $c => $u""".stripMargin
            }

            writer <<
              s"""
                 |  }""".stripMargin
          }
          writer <<
            s"""
               |}""".stripMargin
        }

        //***** Dot trait *****
        writer <<
          s"""
             |
             |trait ${className}Dot[A]{
             |  import ${className}Unit._
             |
             |  protected def ${id}Dot(unit: ${className}Unit): A
             |""".stripMargin

        contextless.foreach { ns =>
          writer <<
            // def m(dot: Dot): A = lengthDot(Length.Metre)
            s"""
               |  def ${ns.symbol}(dot: Dot): A = ${id}Dot(${ns.name})""".stripMargin
        }

        writer <<
          s"""
             |}""".stripMargin

        //***** Per trait *****
        writer <<
          s"""
             |
             |trait ${className}Per[A]{
             |  import ${className}Unit._
             |
             |  protected def ${id}Per(unit: ${className}Unit): A
             |""".stripMargin

        contextless.foreach { ns =>
          writer <<
            // def m(per: Per): A = lengthPer(LengthUnit.Metre)
            s"""
               |  def ${ns.symbol}(per: Per): A = ${id}Per(${ns.name})""".stripMargin
        }

        writer <<
          s"""
             |}""".stripMargin

        //***** Predefined unit trait and object *****
        writer <<
          s"""
             |
             |trait Predefined${className}Unit extends ${className}PostfixOps[${className}Unit]{
             |  override protected def ${id}PostfixOps(unit: ${className}Unit) = unit
             |  ${if(className == "Angle")"override def ° = AngleUnit.Degree" else ""}
             |}
             |
             |object Predefined${className}Unit extends Predefined${className}Unit
             |""".stripMargin
      }
    }
  }

  def fileExtensionFilter(ext: String): Path => Boolean = _.getFileName.toString.endsWith(ext)

  case class UnitConstant(name: String, symbols: Seq[String], args:String, isNotExact: Boolean, mixed: String)

  def getUnitConstants(json: JsonObject): List[UnitConstant] =
    getChildren(json, "constants").map(_.getAsJsonObject).flatMap{ e =>
      val name = getString(e, "name")
      val args = getString(e, "args")
      val isNotExact = getBoolean(e, "NotExact")
      val excludePrefixes = getChildren(e, "excludePrefixes").map(_.getAsString)
      val mixed = if(e.has("mixed"))getString(e, "mixed") else ""

      e match {
        case _ if getBoolean(e, "scalePrefixes") =>
          val symbol = getString(e, "symbol")
          scalePrefixes.filterNot(sp => excludePrefixes.contains(sp.prefix)).map { sp =>
            val symbols =
              if (sp.name == "Micro"){
                if(symbol.length == 1 && symbol.head.isLower)
                  Seq("micro" + name, sp.prefix + symbol)
                // MicroMetre => Seq("microMetre", "μm")
                else
                  Seq("micro" + name, "micro" + headToUpper(symbol), sp.prefix + symbol)
                // MicroMetre => Seq("microGauss", "microG", "μG")
              }else{
                Seq(sp.prefix + symbol)
              }
            UnitConstant(sp.name + name, symbols, s"""r"${sp.scale}"$args""", isNotExact, mixed)
          }

        case _ =>
          Seq(UnitConstant(name, getChildren(e, "symbols").map(_.getAsString), args, isNotExact, mixed))
      }
    }

  def getUnitNameSymbolTuples(units: List[UnitConstant]): (List[NameSymbol], List[NameSymbol]) =
    units.flatMap(u => u.symbols.map(NameSymbol(u.name, _)))
      .partition(_.symbol.contains("("))

  def groupingContextList(contextful: List[NameSymbol],
                          contextList: List[NameSymbol]): Map[String, List[(String, String)]] =

    contextful.map{ ns =>  // NameSymbol("Foot_US_Survey", "ft(US)")
      val ss = ns.symbol.split("\\(")
      (ss(0), ss(1).replaceAll("\\)", ""), ns.name)  // ("ft", "US", "Foot_US_Survey")
    }.groupBy(_._1).map{ g =>
      val symbol = g._1  // "ft"
      val cs = g._2.map(s => (contextList.find(_.symbol == s._2).get.name, s._3))
        // List(("UnitedStates", "Foot_US_Survey"), ...)
      (symbol, cs)
    }

  // Utility methods
  // String
  def headToLower(className: String) = className.charAt(0).toLower.toString + className.substring(1)
  def headToUpper(className: String) = className.charAt(0).toUpper.toString + className.substring(1)
  def quote(s: String) = "\"" + s + "\""

  // Gson
  def getBoolean(e: JsonObject, name: String): Boolean = e.has(name) && e.getAsJsonPrimitive(name).getAsBoolean
  def getString(e: JsonObject, name: String): String = e.getAsJsonPrimitive(name).getAsString
  def getChildren(e: JsonObject, name: String): List[JsonElement] =
    if(e.has(name))e.getAsJsonArray(name).toList
    else Nil
}