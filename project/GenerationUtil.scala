import play.api.libs.json.{JsError, JsResult, JsSuccess, JsValue, Json}
import sbt.io.IO

import java.io.File
import java.nio.charset.Charset
import scala.util.matching.Regex

object GenerationUtil{

  val rootPackage: String = "org.waman.multiverse"
  val utf8: Charset = Charset.forName("UTF-8")

  def readJson[A](jsonFile: File, f: JsValue => JsResult[A]): A =
    f(Json.parse(IO.read(jsonFile, utf8))) match {
      case JsSuccess(result, _) => result
      case JsError(errors) => throw new RuntimeException(errors.mkString("\n"))
    }

  val regexCompositeUnit: Regex = """(\w+)\s*([*/])\s*(\w+)""".r

  private val regexNum: Regex = """(-)?((\d+/\d+)|(\d+(\.\d+)?(e(-)?\d+)?))""".r
  private val regexConst: Regex = """Constants\.\w+""".r
  def refineFactor(s: String): String = s match {
    // TODO
    case "log(2)" => "Real(2).log()"  // Entropy.bit
    case "log(10)" => "Real(10).log()"  // Entropy.ban
    case "sqrt(1/10)" => """Real("1/10").sqrt()"""  // Length.metric_foot
    case _ => 
      val t = regexNum.replaceAllIn(s, m => s"""r"${s.substring(m.start, m.end)}"""")
      regexConst.replaceAllIn(t, m => {
        "Constants." + toCamelCase(t.substring(m.start+10, m.end), "_")  // 10 is length of "Constants."
      })
  }

  val regexUnitName: Regex = """(?:(\w+)\.)?(\w+(?:\(\w+\))?)""".r  // metre, Length.metre, Length.mile(US) etc.

  // "Length.metre^2 => "metre"
  def refineUnitNamesInPoweredBaseUnit(s: String): String = {
    val (start, end) = (s.indexOf('.')+1, s.indexOf('^'))
    val ss = s.substring(start, end) // "metre"
    escape(ss)
  }

  def toSeq[A](array: Array[A]): Seq[A] = if (array != null) array.toList else Nil

  def toCamelCase(s: String, sep: String): String = 
    s.split(sep).map(s => s(0).toUpper + s.substring(1)).mkString("")

  def toSnakeCase(s: String): String = escape(s.replace(' ', '_'))

  private val escapes = "()²³°℧₂℃℉"

  def escape(s: String): String =
    if (escapes.exists(s.contains(_))) s"""`$s`""" else s

  def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)
}