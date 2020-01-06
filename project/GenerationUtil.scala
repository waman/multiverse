import java.nio.charset.Charset

import com.google.gson.Gson

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

  def extractUnitTypes(s: String): Seq[String] = regexUnitName.findAllMatchIn(s).map{ m =>
    val str = s.substring(m.start, m.end)
    str.indexOf('.') match {
      case -1 => Nil
      case i => Seq(str.substring(0, i))
    }
  }.toSeq.flatten

  def refineUnitNamesInBaseUnit(s: String): String = refineUnitNames(s, (prefix, unitName) => {
    val (baseUN, power) =
      if (unitName.endsWith("^2")) (unitName.substring(0, unitName.length-2), 2)
      else if (unitName.endsWith("^3")) (unitName.substring(0, unitName.length-2), 3)
      else (unitName, 1)

    val escapedUN = if (baseUN.contains('(')) s"`$baseUN`" else baseUN

    power match {
      case 2 | 3 => s"$prefix$escapedUN.interval**$power"
      case _ => s"$prefix$escapedUN.interval"
    }
  })

  def refineUnitNamesInConvertible(s: String): String = refineUnitNames(s, (prefix, unitName) => {
    val escapedUN = if (unitName.contains('(')) s"`$unitName`" else unitName
    s"$prefix$escapedUN"
  })

  private def refineUnitNames(s: String, extraOperation: (String, String) => String): String =
    regexUnitName.replaceAllIn(s, m => {
      val str = s.substring(m.start, m.end)
      val (prefix, unitName) = str.indexOf('.') match {
        case -1 => ("", str)
        case i => (str.substring(0, i) + "UnitObjects.", str.substring(i + 1))
      }
      extraOperation(prefix, unitName)
    })

  def toSeq[A](array: Array[A]): Seq[A] = if (array != null) array.toList else Nil

  def toObjectName(s: String): String = {
    val ss = s.replace(' ', '_')
    if (ss.contains("(")) s"""`$ss`"""
    else ss
  }

  def escapeSymbol(s: String): String =
    if (s.matches("""\w+""")) s
    else s"""`$s`"""

  def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)
}