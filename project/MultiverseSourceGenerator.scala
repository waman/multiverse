import java.nio.file.Path

import GenerationUtil._
import com.google.gson._
import com.google.gson.reflect.TypeToken
import org.waman.gluino.io.FileType
import org.waman.gluino.nio.GluinoPath

object MultiverseSourceGenerator extends GluinoPath{

  val gson = new Gson

  val contextType = new TypeToken[Array[ContextJson]](){}.getRawType
  val unitType = new TypeToken[UnitJson](){}.getRawType

  def generate(rsrc: Path, srcManaged: Path): List[Path] = {
    srcManaged.createDirectories()

    val (contexts, contextArrays) = generateContexts(rsrc, srcManaged)
    // contextArrays: Map("org.waman.multiverse.metric.MetricContext" -> Array(NameSymbol("UnitedStates", "US"), NameSymbol("Imperial", "imp"), ...)

    val (unitSystem, unitSourceArray) = generateUnitSystem(rsrc, srcManaged, contextArrays)

    val unitTraits = generateUnitTraits(rsrc, srcManaged, contextArrays, unitSourceArray)

    contexts ::: unitSystem :: unitTraits
  }

  def generateContexts(rsrc: Path, srcManaged: Path): (List[Path], Map[String, Array[ContextJson]]) = {

    val seq = rsrc.filesMatchRecurse(FileType.Files, fileExtensionFilter("Context.json")).map { json =>
      val id = json.getFileName.toString.replaceAll("\\.json", "")
      val rel = rsrc.relativize(json.getParent)
      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")
      val generated = srcManaged / rel / (id + ".scala")

      val nsArray = json.withReader(gson.fromJson(_, contextType)).asInstanceOf[Array[ContextJson]]

      if(!generated.exists || generated.isOlderThan(json) ) {
        if (generated.exists) generated.delete()
        generated.getParent.createDirectories()

        generateContext(id, packageName, generated, nsArray)
        println("[GENERATE] " + generated)
      }

      (generated, packageName + "." + id, nsArray)
      // (generated, "org.waman.multiverse.metric.MetricContext", Array(NameSymbol("UnitedStates", "US"), ...))
    }

    (seq.map(_._1).toList, Map(seq.map(t => (t._2, t._3)):_*))
  }

  def generateContext(id: String, packageName: String, generated: Path, contextArray: Array[ContextJson]): Unit = {
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

      contextArray.foreach { ns =>
        writer <<
          // case object UnitedStates extends Context("UnitedStates", "US")
          s"""
             |  case object ${ns.name} extends $id("${ns.name}", "${ns.symbol}")""".stripMargin
      }

      writer <<
        s"""
           |
           |  lazy val values = Seq(${contextArray.map(_.name).mkString(", ")})
           |}""".stripMargin

      // ContextDefined trait
      writer <<
        s"""
           |
           |trait ${id}Defined{
           |  import $id._
           |""".stripMargin

      contextArray.foreach{ ns =>
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

  def generateUnitSystem(rsrc: Path, srcManaged: Path, contextLists: Map[String, Array[ContextJson]]):
      (Path, Array[UnitSource]) = {

    val generated = srcManaged / "org/waman/multiverse/UnitSystem.scala"

    val unitSystemInfo = jsonPropertyFilesOfUnits(rsrc).map { json: Path =>
      val rel = rsrc.relativize(json.getParent)
      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")
      val className = json.getFileName.toString.replace("Unit.json", "")
      val fullClassName = s"$packageName.$className"

      val prop = json.withReader(gson.fromJson(_, unitType)).asInstanceOf[UnitJson]
      val us = UnitSource(json, packageName, className, prop)

      if (prop.hasConstants)
        List(us, fullClassName, s"$packageName.Predefined${className}Unit")
      else
        List(us, fullClassName)
    }

    if(!generated.exists || jsonPropertyFilesOfUnits(rsrc).forall(generated.isOlderThan)) {
      if (generated.exists) generated.delete()

      generated.withWriter { writer =>
        writer <<
          s"""package org.waman.multiverse
             |
             |trait UnitSystem extends UnitSystemImplicits""".stripMargin

        contextLists.foreach{ case (contextClass, _) =>
          writer <<
            s"""
               |  with ${contextClass}Defined""".stripMargin
        }

        unitSystemInfo.flatMap {
          case list if list.length == 3 => List(list(2))
          case _ => Nil
        }.foreach{ predef =>
          writer <<
              s"""
                 |  with $predef""".stripMargin
        }

        writer <<
          s"""
             |
             |object UnitSystem extends UnitSystem{
             |
             |  lazy val supportedQuantities: Set[Class[_]] = Set(
             |    """.stripMargin

        writer << unitSystemInfo.map(_(1)).map(
          "classOf[" + _ + "[_]]").mkString(
          s""",
             |    """.stripMargin)
        writer <<
          s"""
             |  )
             |}""".stripMargin
       }
       println("[GENERATE] " + generated)
    }

    val props = unitSystemInfo.map(_(0).asInstanceOf[UnitSource]).toArray
    (generated, props)
  }

  def generateUnitTraits(rsrc: Path, srcManaged: Path,
                         contextArrays: Map[String, Array[ContextJson]],
                         unitSourceArray: Array[UnitSource]): List[Path] = {
    jsonPropertyFilesOfUnits(rsrc).map{ json: Path =>
      val unitSource = unitSourceArray.find(_.src == json).get

      val rel = rsrc.relativize(json.getParent)
      val destDir = srcManaged / rel
      val fileName = if(unitSource.unitName == "Temperature") "TemperaturePostfixOps.scala"
                     else unitSource.unitName + "Unit.scala"
      val generated = destDir / fileName

      if(!generated.exists || generated.isOlderThan(json)){
        if(generated.exists) generated.delete()
        destDir.createDirectories()
        generated.createFile()

        generateUnitTrait(generated, unitSource.packageName, unitSource.unitName, unitSource.unitJson, contextArrays)
        println("[GENERATE] " + generated)
      }
      generated
    }
  }

  def generateUnitTrait(dest: Path, packageName: String, className: String,
                        json: UnitJson, contextArrays: Map[String, Array[ContextJson]]): Unit = {
    val id = headToLower(className)  // length
    val constants: Array[CanonicalConstant] = json.getCanonicalConstants
    val (contextful, contextless) = getUnitNameSymbolTuples(constants)
    // contextful: Array(NameSymbol("Foot", "ft(US)"), ...)
    // contextless: Array(NameSymbol("Metre", "m"), NameSymbol("Foot", "ft"), ...)

    dest.withWriter { writer =>

      writer <<
        s"""package $packageName
           |
           |import spire.math.Real
           |import spire.implicits._
           |import org.waman.multiverse._
           |""".stripMargin

      json.getImports.foreach{ i =>
        writer <<
          s"""
             |import $i""".stripMargin
      }

      if(className != "Temperature"){
        //***** Unit trait  *****
        val siUnit = json.SIUnit match {
          case s if s.contains("*") || s. contains("/") => s
          case s => packageName + "." + className + "Unit." + s
        }

        val muls = json.getMultiplicatives.map(m => (m(0), m(1)))
        val divs = json.getDivisibles.map(d => (d(0), d(1)))

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

        if(json.hasSquare){
          writer <<
            s"""
               |  with CanSquare[${json.square}]""".stripMargin
        }

        if(json.hasCubic){
          writer <<
            s"""
               |  with CanCubic[${json.cubic}]""".stripMargin
        }

        writer <<
          s"""{
             |
             |  override def getSIUnit = $siUnit""".stripMargin

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

        if(json.hasSquare){
          writer <<
            s"""
               |
               |  override def square: ${json.square} = this * this""".stripMargin
        }

        if(json.hasCubic){
          writer <<
            s"""
               |
               |  override def cubic: ${json.cubic} = this * this * this""".stripMargin
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
             |  class Intrinsic${className}Unit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
             |      extends ${className}Unit{
             |
             |    def this(name: String, symbols: Seq[String], unit: ${className}Unit) =
             |      this(name, symbols, unit.unitValueInSIUnit)
             |
             |    def this(name: String, symbols: Seq[String], factor: Real, unit: ${className}Unit) =
             |      this(name, symbols, factor * unit.unitValueInSIUnit)
             |  }
             |
             |""".stripMargin

        constants.foreach{ unit =>
          //  case object Metre extends IntrinsicLengthUnit("Metre", Seq("m"), r"1")
          writer <<
            s"""
               |  case object ${unit.name} extends Intrinsic${className}Unit""".stripMargin
          writer << s"""("${unit.name}", ${unit.symbolSeqString}, ${unit.args})"""
          if(unit.isNotExact) writer << " with NotExact"
          if(unit.hasMixed){
            writer <<
              s"""
                 |    ${unit.mixed}""".stripMargin  // for Degree
          }
        }

        writer <<
          //  Seq(..., MilliMetre, CentiMetre, Metre, ...)
          s"""
             |
             |  override lazy val values = Seq(${constants.map(_.name).mkString(", ")})""".stripMargin


        val prods = json.getProducts.map(p => (p(0), p(1)))
        val quots = json.getQuotients.map(q => (q(0), q(1)))

        // product unit
        prods.foreach{ case (first, second) =>
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
               |    override lazy val unitValueInSIUnit: Real =
               |      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
               |  }
               |
               |  def apply(unit1: $first, unit2: $second): ${className}Unit =
               |    new $productUnit(unit1, unit2)""".stripMargin
        }

        // quotient unit
        quots.foreach { case (numerator, denominator) =>
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
               |    override lazy val unitValueInSIUnit: Real =
               |      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
               |  }
               |
               |  def apply(nUnit: $numerator, dUnit: $denominator): ${className}Unit =
               |    new $quotientUnit(nUnit, dUnit)""".stripMargin
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

      if(constants.nonEmpty) {
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

        val contextFullClassName = json.contextFullClassName
        val contextClassName = json.contextClassName

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
          val contextList = contextArrays(contextFullClassName)
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
            //   Array(("UnitedStates", "Foot_US_Survey"), ...)
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

  def getUnitNameSymbolTuples(units: Array[CanonicalConstant]): (Array[ContextJson], Array[ContextJson]) =
    units.flatMap(u => u.symbols.map(ContextJson(u.name, _)))
      .partition(_.symbol.contains("("))

  def groupingContextList(contextful: Array[ContextJson],
                          contextList: Array[ContextJson]): Map[String, Array[(String, String)]] =

    contextful.map{ ns =>  // NameSymbol("Foot_US_Survey", "ft(US)")
      val ss = ns.symbol.split("\\(")
      (ss(0), ss(1).replaceAll("\\)", ""), ns.name)  // ("ft", "US", "Foot_US_Survey")
    }.groupBy(_._1).map{ g =>
      val symbol = g._1  // "ft"
      val cs = g._2.map(s => (contextList.find(_.symbol == s._2).get.name, s._3))
        // Array(("UnitedStates", "Foot_US_Survey"), ...)
      (symbol, cs)
    }
}