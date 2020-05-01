import java.io.{BufferedWriter, File}
import java.nio.charset.Charset

import com.google.gson.Gson
import sbt.io.IO

import scala.util.matching.Regex

object GenerationUtil{

  val rootPackage: String = "org.waman.multiverse"
  val gson: Gson = new Gson
  val utf8: Charset = Charset.forName("UTF-8")

  val regexId: Regex = """[a-zA-z.]+""".r
  val regexCompositeUnit: Regex = """(\w+)\s*([*/])\s*(\w+)""".r

  private val regexNum: Regex = """(-)?\d+(\.\d+)?(e(-)?\d+)?""".r
  def refineNumbers(s: String): String = s match {
    // TODO
    case "log(2)" => "Real(2).log()"  // Entropy.bit
    case "log(10)" => "Real(10).log()"  // Entropy.ban
    case "sqrt(1/10)" => """Real("1/10").sqrt()"""  // Length.metric_foot
    case _ => regexNum.replaceAllIn(s, m => s"""r"${s.substring(m.start, m.end)}"""")
  }

  private val regexUnitName: Regex = """[\w.()^\d]+""".r

  def allFiles(dir: File): Seq[File] = {
    def allFiles(f: File, acc: Seq[File]): Seq[File] =
      if (f.isFile)
        f +: acc
      else if (f.isDirectory)
        IO.listFiles(f).toList.flatMap(allFiles(_, acc))
      else
        acc

    allFiles(dir, Nil)
  }

  def lastModifiedIn(dir: File): Long = allFiles(dir) match {
    case Nil => -1
    case files => files.map(_.lastModified).max
  }

  def foreachUnitDefinition(ids: Seq[String], jsons: JsonResources)(f: UnitDefinitionJson => Unit): Unit =
    ids.distinct.map(jsons.searchUnitDefinition).foreach(f)

  /** "Length.metre / Time.second" => Seq((Length, metre), (Time, second)) */
  def extractUnitTypes(s: String): Seq[(String, String)] = regexUnitName.findAllMatchIn(s).map{ m =>
    val str = s.substring(m.start, m.end)
    str.indexOf('.') match {
      case -1 => Nil
      case i => Seq((str.substring(0, i), str.substring(i+1)))
    }
  }.toSeq.flatten

  /** "Length.metre / Time.second" => "LengthUnitObjects.metre.interval / TimeUnitObjects.second.interval" */
  def refineUnitNamesInBaseUnit(s: String): String = refineUnitNames(s, (id, unit) => {
    val (baseUN, power) =
      if (unit.endsWith("^2")) (unit.substring(0, unit.length-2), 2)
      else if (unit.endsWith("^3")) (unit.substring(0, unit.length-2), 3)
      else (unit, 1)

    val escapedUN = if (baseUN.contains('(')) s"`$baseUN`" else baseUN

    power match {
      case 2 | 3 => s"$escapedUN.interval**$power"
      case _ => s"$escapedUN.interval"
    }
  })

  def refineUnitNamesInConvertible(s: String): String = refineUnitNames(s, (id, unit) => {
    val escapedUN = if (unit.contains('(')) s"`$unit`" else unit
    s"${id}UnitObjects.$escapedUN"
  })

  def refineUnitNamesInUnitSystem(s: String): String = refineUnitNames(s, (_, unit) => unit)

  private def refineUnitNames(s: String, extraOperation: (String, String) => String): String =
    regexUnitName.replaceAllIn(s, m => {
      val str = s.substring(m.start, m.end)
      val (id, unit) = str.indexOf('.') match {
        case -1 => ("", str)
        case i => (str.substring(0, i), str.substring(i + 1))
      }
      extraOperation(id, unit)
    })

  def isCompositeUnit(s: String): Boolean = s.contains('*') || s.contains('/')

  def toSeq[A](array: Array[A]): Seq[A] = if (array != null) array.toList else Nil

  def toObjectName(s: String): String = {
    val ss = s.replace(' ', '_')
//    if (ss.contains("(")) s"""`$ss`"""
//    else ss
    escapeSymbol(ss)
  }

  def escapeSymbol(s: String): String =
    if (s.matches("""\w+""")) s
    else s"""`$s`"""

  def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)

  def isOptionalAliase(a: String): Boolean = a.startsWith("(") && a.endsWith(")")

  // [gregorian: Seq(month, year, decade, ...), julian: Seq(year, decade, ...), ...]
  def extractAttributeMap[U <: UnitInfo](attUnits: Seq[U]): Map[String, Seq[String]] =
    attUnits.flatMap(u => u.attributes.map(a => (u.objectName, a.name)))
    .groupBy(_._2)
    .mapValues(v => v.map(_._1))
}