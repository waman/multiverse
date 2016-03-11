import java.io.File
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
    Scale("Deca" , "da", "1e-1"),
    Scale("Hecto", "h", "1e-2"),
    Scale("Kilo" , "k", "1e-3"),
    Scale("Mega" , "M", "1e-6"),
    Scale("Giga" , "G", "1e-9"),
    Scale("Tera" , "T", "1e-12"),
    Scale("Peta" , "P", "1e-15"),
    Scale("Exa"  , "E", "1e-18"),
    Scale("Zetta", "Z", "1e-21"),
    Scale("Yotta", "Y", "1e-24")
  )

  def generate(rsrc: File, srcManaged: File): Seq[File] = {
    val rsrcPath = rsrc.toPath
    val srcManagedPath = srcManaged.toPath
    srcManagedPath.createDirectories()

    val (contextCodePath, contextList) = generateContext(rsrcPath, srcManagedPath)
          // Context.scala, List(NameSymbol("UnitedStates", "US"), NameSymbol("Imperial", "imp"), ...)

    (contextCodePath :: generateUnitTraits(rsrcPath, srcManagedPath, contextList))
      .map(_.toFile)
  }

  def generateContext(rsrc: Path, srcManaged: Path): (Path, List[NameSymbol]) = {
    val json = rsrc / "org/waman/multiverse/Context.json"
    val contextList = json.withReader(parser.parse(_))
                        .getAsJsonArray.toList
                        .map(_.getAsJsonObject)
                        .map{ obj => NameSymbol(getString(obj, "name"), getString(obj, "symbol")) }

    val generated = srcManaged / "org/waman/multiverse/Context.scala"
    if(!generated.exists || generated.isOlderThan(json) ){
      generated.getParent.createDirectories()
      generated.createFile()

      generated.withWriter{ writer =>

        // Context trait
        writer <<
          s"""package org.waman.multiverse
             |
             |sealed abstract class Context(val name: String, val symbol: String)""".stripMargin

        // Context object
        writer <<
          s"""
             |
             |/** The "US" context contains the "US Survey" one for Length and Area (i.e. ft(US) and mi(US)) */
             |object Context extends ConstantsDefined[Context]{
             |""".stripMargin

        contextList.foreach { ns =>
          writer <<
            // case object UnitedStates extends Context("UnitedStates", "US")
            s"""
               |  case object ${ns.name} extends Context("${ns.name}", "${ns.symbol}")""".stripMargin
        }

        writer <<
          s"""
             |  lazy val values = Seq(${contextList.map(_.name).mkString(", ")})
             |}""".stripMargin

        // HasContext trait
        writer <<
          s"""
             |
             |trait HasContext{
             |  import Context._
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
      println("[GENERATE] " + generated)
    }
    (generated, contextList)
  }

  def generateUnitTraits(rsrc: Path, srcManaged: Path, contextList: List[NameSymbol]): List[Path] = {
    rsrc.filesMatchRecurse(FileType.Files, fileExtensionFilter(_, "Unit.json")).map{ json: Path =>
      val rel = rsrc.relativize(json.getParent)
      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")

      val destDir = srcManaged / rel
      val className = json.getFileName.toString.replace("Unit.json", "")  // Length (not contain "Unit")
      val fileName = className + "Unit.scala"
      val generated = destDir / fileName

      if(!generated.exists || generated.isOlderThan(json)){
        destDir.createDirectories()
        generated.createFile()

        val jsonProp: JsonObject = json.withReader(parser.parse(_)).getAsJsonObject
        generateUnitTrait(generated, packageName, className, jsonProp, contextList)
        println("[GENERATE] " + generated)
      }
      generated
    }.toList
  }

  def fileExtensionFilter(path: Path, ext: String): Boolean = path.getFileName.toString.endsWith(ext)

  def generateUnitTrait(dest: Path, packageName: String, className: String,
                        json: JsonObject, contextList: List[NameSymbol]): Unit = {
    val id = headToLower(className)  // length
    val imports = getChildren(json, "imports").map(_.getAsString)
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

      imports.foreach{ i =>
        writer <<
          s"""
             |import $i""".stripMargin
      }

      //***** PostfixOps trait *****
      writer <<
        s"""
           |
           |trait ${className}PostfixOps[A]{
           |  import ${className}Unit._
           |
           |  protected def ${id}PostfixOps(unit: ${className}Unit): A
           |""".stripMargin

      contextless.foreach{ ns =>
        writer <<
          // def m: A = lengthPostfixOps(LengthUnit.Metre)
          s"""
             |  def ${ns.symbol}: A = ${id}PostfixOps(${ns.name})""".stripMargin
      }

      if(contextful.nonEmpty){
        writer <<
          s"""
             |  import ${className}PostfixOps._
             |""".stripMargin

        contextful.foreach{ ns =>
          val sym = ns.symbol.split("\\(")(0)
          writer <<
            // def ft(c: Context): A = lengthPostfixOps(_ft(c))
            s"""
               |  def $sym(c: Context): A = ${id}PostfixOps(_$sym(c))""".stripMargin
        }
      }

      writer <<
        s"""
           |}""".stripMargin

      //***** PostfixOps object *****
      if(contextful.nonEmpty){
        writer <<
          s"""
             |
             |object ${className}PostfixOps{
             |  import ${className}Unit._
             |  import org.waman.multiverse.Context._
             |""".stripMargin

        groupingContextList(contextful, contextList).foreach{ case (symbol, cs) =>
          // (
          //   "ft",
          //   List(("UnitedStates", "Foot_US_Survey"), ...)
          // )

          writer <<
            s"""
               |  lazy val _$symbol: PartialFunction[Context, ${className}Unit] = {
               |""".stripMargin

          cs.foreach{ c =>
            writer <<
              // case US => Foot_US_Survey
              s"""
                 |    case ${c._1} => ${c._2}""".stripMargin
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

      contextless.foreach{ ns =>
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

      contextless.foreach{ ns =>
        writer <<
          // def m(per: Per): A = lengthPer(LengthUnit.Metre)
          s"""
             |  def ${ns.symbol}(per: Per): A = ${id}Per(${ns.name})""".stripMargin
      }

      writer <<
        s"""
           |}""".stripMargin

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

      muls.foreach{ m =>
        writer <<
          s"""
             |  with MultiplicativeBy${m._1}Unit[${m._2}Unit]""".stripMargin
      }

      divs.foreach{ d =>
        writer <<
          s"""
              |  with DivisibleBy${d._1}Unit[${d._2}Unit]""".stripMargin
      }

      writer <<
        s"""{
           |
           |  def $baseUnitAccessor: Real
           |
           |  override def baseUnit = $baseUnit
           |  override def valueInBaseUnit = $baseUnitAccessor""".stripMargin

      muls.foreach{ m =>
        writer <<
          s"""
             |
             |  override def *(unit: ${m._1}Unit) = ${m._2}Unit(this, unit)""".stripMargin
      }

      divs.foreach{ d =>
        writer <<
          s"""
             |
             |  override def /(unit: ${d._1}Unit) = ${d._2}Unit(this, unit)""".stripMargin
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
           |""".stripMargin

      units.foreach{ unit =>
        //  case object Metre extends IntrinsicLengthUnit("Metre", Seq("m"), r"1")
        writer << s"""
                     |  case object ${unit.name} extends Intrinsic${className}Unit""".stripMargin
        writer << s"""("${unit.name}", Seq(${unit.symbols.map(quote).mkString(", ")}), ${unit.args})"""
        if(unit.isNotExact) writer << " extends NotExact"
      }

      writer <<
        //  Seq(..., MilliMetre, CentiMetre, Metre, ...)
        s"""
           |
           |  override lazy val values = Seq(${units.map(_.name).mkString(", ")})""".stripMargin

      // product unit
      if(json.has("product")) {
        val List(first, second) = getChildren(json, "product").map(_.getAsString)
        writer <<
          s"""
             |
             |  // $first * $second -> $className
             |  private[${className}Unit]
             |  class Product${className}Unit(val firstUnit: ${first}Unit, val secondUnit: ${second}Unit)
             |      extends ${className}Unit with ProductUnit[${className}Unit, ${first}Unit, ${second}Unit]{
             |
             |    override lazy val $baseUnitAccessor: Real =
             |      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
             |  }
             |
             |  def apply(unit1: ${first}Unit, unit2: ${second}Unit): ${className}Unit =
             |    new Product${className}Unit(unit1, unit2)""".stripMargin
      }

      // quotient unit
      if(json.has("quotient")) {
        val List(numerator, denominator) = getChildren(json, "quotient").map(_.getAsString)
        writer <<
          s"""
             |
             |  // $numerator / $denominator -> $className
             |  private[${className}Unit]
             |  class Quotient${className}Unit(val numeratorUnit: ${numerator}Unit, val denominatorUnit: ${denominator}Unit)
             |      extends ${className}Unit with QuotientUnit[${className}Unit, ${numerator}Unit, ${denominator}Unit]{
             |
             |    override lazy val $baseUnitAccessor: Real =
             |      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
             |  }
             |
             |  def apply(nUnit: ${numerator}Unit, dUnit: ${denominator}Unit): ${className}Unit =
             |    new Quotient${className}Unit(nUnit, dUnit)""".stripMargin
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
           |}
           |
           |object Predefined${className}Unit extends Predefined${className}Unit
           |""".stripMargin
    }
  }

  case class UnitConstant(name: String, symbols: Seq[String], args:String, isNotExact: Boolean)

  def getUnitConstants(json: JsonObject): List[UnitConstant] =
    getChildren(json, "constants").map(_.getAsJsonObject).flatMap{ e =>
      val name = getString(e, "name")
      val args = getString(e, "args")
      val isNotExact = getBoolean(e, "NotExact")
      val excludePrefixes = getChildren(e, "excludePrefixes").map(_.getAsString)

      e match {
        case _ if getBoolean(e, "scalePrefixes") =>
          val symbol = getString(e, "symbol")
          scalePrefixes.filterNot(sp => excludePrefixes.contains(sp.prefix)).map { sp =>
            val symbols =
              if (sp.name == "Micro") Seq("micro" + name, "micro" + headToUpper(symbol), sp.prefix + symbol)
                // MicroMetre => Seq("microMetre", "microM", "μm")
              else Seq(sp.prefix + symbol)
            UnitConstant(sp.name + name, symbols, s"""r"${sp.scale}"$args""", isNotExact)
          }

        case _ =>
          Seq(UnitConstant(name, getChildren(e, "symbols").map(_.getAsString), args, isNotExact))
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