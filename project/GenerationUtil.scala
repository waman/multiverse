import com.google.gson.Gson

import java.nio.charset.Charset
import scala.util.matching.Regex

object GenerationUtil{

  val rootPackage: String = "org.waman.multiverse"
  val gson: Gson = new Gson
  val utf8: Charset = Charset.forName("UTF-8")

  val regexCompositeUnit: Regex = """(\w+)\s*([*/])\s*(\w+)""".r

  private val regexNum: Regex = """(-)?((\d+/\d+)|(\d+(\.\d+)?(e(-)?\d+)?))""".r
  def refineNumbers(s: String): String = s match {
    // TODO
    case "log(2)" => "Real(2).log()"  // Entropy.bit
    case "log(10)" => "Real(10).log()"  // Entropy.ban
    case "sqrt(1/10)" => """Real("1/10").sqrt()"""  // Length.metric_foot
    case _ => regexNum.replaceAllIn(s, m => s"""r"${s.substring(m.start, m.end)}"""")
  }

  val regexUnitName: Regex = """(?:(\w+)\.)?(\w+(?:\(\w+\))?)""".r  // metre, Length.metre, Length.mile(US) etc.

  // "Length.metre^2 => "metre"
  def refineUnitNamesInPoweredBaseUnit(s: String): String = {
    val (start, end) = (s.indexOf('.')+1, s.indexOf('^'))
    val ss = s.substring(start, end) // "metre"
    escape(ss)
  }

  def toSeq[A](array: Array[A]): Seq[A] = if (array != null) array.toList else Nil

  def toObjectName(s: String): String = escape(s.replace(' ', '_'))

  private val escapes = "()²³°℧₂℃℉"

  def escape(s: String): String =
    if (escapes.exists(s.contains(_))) s"""`$s`""" else s

  def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)
}