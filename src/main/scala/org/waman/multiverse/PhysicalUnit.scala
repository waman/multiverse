package org.waman.multiverse

import spire.implicits._
import spire.math.Real

import scala.util.matching.Regex

trait PhysicalUnit[U <: PhysicalUnit[U]] {
  def name: String
  def symbol: String
  def aliases: Seq[String]
  def getSIUnit: U
}

// Maybe only temperature
trait HomogeneousUnit[U <: HomogeneousUnit[U]] extends PhysicalUnit[U]{
  /** zero value in SI unit */
  def zero: Real
  /** interval in SI Unit */
  def interval: Real

  /** Use <code>name</code> and <code>interval</code> properties (not <code>symbol</code>) for equality evaluation. */
  override def equals(other: Any): Boolean = other match {
    case that: HomogeneousUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        zero == that.zero &&
        interval == that.interval
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PhysicalUnit[_]]

  override def hashCode: Int =
    41 * (
      41 * (
        41 + name.hashCode
        ) + zero.hashCode
    ) + interval.hashCode

  override def toString: String =
    if(this == getSIUnit) {
      s"$name ($symbol)"
    }else{
      val symbolSI = getSIUnit.symbol
      val sZero = toReadableString(zero)
      this.interval match {
        case i if i.isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = Δ($symbolSI)]"
        case i if (-i).isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = -Δ($symbolSI)]"
        case _ =>
          val sInterval = toReadableString(interval)
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = $sInterval*Δ($symbolSI)]"
      }
  }
}

@deprecated
object PhysicalUnit{

  // pattern like $u00B0
  private val escaped: Regex = """\$u([0-9A-F]{4})""".r

  private[multiverse] def extractObjectSymbol(obj: Any): String = {
    import scala.reflect.runtime.{universe => ru}

    val im = ru.runtimeMirror(getClass.getClassLoader).reflect(obj)
    val s = im.symbol.name.toString

    // transform string like "$u00B0C" to "°C"
    def decode(s: String): String = Integer.parseInt(s, 16).asInstanceOf[Char].toString
    escaped.replaceAllIn(s, m => decode(m.group(1)))
  }
}

@deprecated
trait NameByClassName[U <: PhysicalUnit[U]] extends PhysicalUnit[U]{

  override def name: String = {
    import scala.reflect.runtime.{universe => ru}
    val im = ru.runtimeMirror(getClass.getClassLoader).reflect(this)
    im.symbol.name.toString
  }
}

@deprecated
trait SymbolByClassName[U <: PhysicalUnit[U]] extends PhysicalUnit[U]{
  override lazy val symbol: String = newSymbolString
  protected def newSymbolString: String = PhysicalUnit.extractObjectSymbol(this)
  override def aliases: Seq[String] = Nil
}

trait NotExact