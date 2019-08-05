package org.waman.multiverse

import spire.implicits._
import spire.math.Real

import scala.util.matching.Regex

trait PhysicalUnit[U <: PhysicalUnit[U]]{

  def name: String
  def symbol: String
  def aliases: Seq[String]

  def getSIUnit: U
  def zeroInSIUnit: Real
  def intervalInSIUnit: Real

  /** Use <code>name</code> and <code>unitValueInSIUnit</code> properties (not <code>symbol</code>) for equality evaluation. */
  override def equals(other: Any): Boolean = other match {
    case that: PhysicalUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        zeroInSIUnit == that.zeroInSIUnit &&
        intervalInSIUnit == that.intervalInSIUnit
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PhysicalUnit[_]]

  override def hashCode: Int =
    41 * (
      41 * (
        41 + name.hashCode
        ) + zeroInSIUnit.hashCode
    ) + intervalInSIUnit.hashCode

  override def toString: String =
    if(this == getSIUnit) {
      s"$name ($symbol)"
    }else{
      val symbolSI = getSIUnit.symbol
      val sZero = toReadableString(zeroInSIUnit)
      this.intervalInSIUnit match {
        case i if i.isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = Δ($symbolSI)]"
        case i if (-i).isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = -Δ($symbolSI)]"
        case _ =>
          val sInterval = toReadableString(intervalInSIUnit)
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = $sInterval*Δ($symbolSI)]"
      }
  }
}

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

// For symbol property of QuotientUnit: m/(s*ms)
// Tests are written in AccelerationSpec
private[multiverse] trait LiteralComposite

trait ProductUnit[U <: PhysicalUnit[U], A <: PhysicalUnit[A], B <: PhysicalUnit[B]]
    extends PhysicalUnit[U] with LiteralComposite {

  def firstUnit: A
  def secondUnit: B

  override lazy val name: String = s"${firstUnit.name} times ${secondUnit.name}"
  override lazy val symbol: String = s"${firstUnit.symbol}*${secondUnit.symbol}"
  override def aliases: Seq[String] = {
    val fm = firstUnit.symbol +: firstUnit.aliases
    (secondUnit.symbol +: secondUnit.aliases).flatMap(s => fm.map(f => s"$f*$s")).tail
  }

  override val intervalInSIUnit: Real = firstUnit.intervalInSIUnit * secondUnit.intervalInSIUnit

  override def equals(other: Any): Boolean = other match {
    case that: ProductUnit[_, _, _] =>
      (that canEqual this) &&
      firstUnit == that.firstUnit &&
      secondUnit == that.secondUnit
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[ProductUnit[_, _, _]]

  override def hashCode: Int =
    41 * (
      41 * (
        41 + firstUnit.hashCode
        ) + "*".hashCode
    ) + secondUnit.hashCode
}

trait QuotientUnit[U <: PhysicalUnit[U], A <: PhysicalUnit[A], B <: PhysicalUnit[B]]
  extends PhysicalUnit[U] with LiteralComposite {

  def numeratorUnit: A
  def denominatorUnit: B

  override lazy val name: String = s"${numeratorUnit.name} per ${denominatorUnit.name}"
  override lazy val symbol: String =
    if(denominatorUnit.isInstanceOf[LiteralComposite])
      s"${numeratorUnit.symbol}/(${denominatorUnit.symbol})"
    else
      s"${numeratorUnit.symbol}/${denominatorUnit.symbol}"

  override def aliases: Seq[String] = {
    val nm = numeratorUnit.symbol +: numeratorUnit.aliases
    (denominatorUnit.symbol +: denominatorUnit.aliases).flatMap(d => nm.map(n => s"$n/$d")).tail
  }

  override val intervalInSIUnit: Real = numeratorUnit.intervalInSIUnit / denominatorUnit.intervalInSIUnit

  override def equals(other: Any): Boolean = other match {
    case that: QuotientUnit[_, _, _] =>
      (that canEqual this) &&
        numeratorUnit == that.numeratorUnit &&
        denominatorUnit == that.denominatorUnit
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[QuotientUnit[_, _, _]]

  override def hashCode: Int =
    41 * (
      41 * (
        41 + numeratorUnit.hashCode
        ) + "/".hashCode
    ) + denominatorUnit.hashCode
}