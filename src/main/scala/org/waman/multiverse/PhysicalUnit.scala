package org.waman.multiverse

import spire.implicits._
import spire.math.Real

trait PhysicalUnit[U <: PhysicalUnit[U]]{

  def name: String
  lazy val symbol: String = newSymbolString

  protected def newSymbolString: String = extractObjectSymbol(this)

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
      this.intervalInSIUnit match {
        case i if i.isOne =>
          s"$name ($symbol) [0($symbol) = $zeroInSIUnit($symbolSI), Δ($symbol) = Δ($symbolSI)]"
        case i if (-i).isOne =>
          s"$name ($symbol) [0($symbol) = $zeroInSIUnit($symbolSI), Δ($symbol) = -Δ($symbolSI)]"
        case _ =>
          s"$name ($symbol) [0($symbol) = $zeroInSIUnit($symbolSI), Δ($symbol) = $intervalInSIUnit*Δ($symbolSI)]"
      }
  }
}

trait ScaleUnit[U <: ScaleUnit[U]] extends PhysicalUnit[U] with Ordered[U]{

  override def zeroInSIUnit: Real = 0

  /** Use <code>name</code> and <code>unitValueInSIUnit</code> properties (not <code>symbol</code>) for equality evaluation. */
  override def equals(other: Any): Boolean = other match {
    case that: ScaleUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        intervalInSIUnit == that.intervalInSIUnit
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[ScaleUnit[_]]

  override def hashCode: Int =
    41 * (
      41 + name.hashCode
      ) + intervalInSIUnit.hashCode

  override def toString: String = this match {
    case thisUnit if thisUnit == getSIUnit =>
      s"$name ($symbol)"
    case _: NotExact =>
      s"$name ($symbol) [1($symbol) ≈ $intervalInSIUnit(${getSIUnit.symbol})]"
    case _ =>
      s"$name ($symbol) [1($symbol) = $intervalInSIUnit(${getSIUnit.symbol})]"
  }

  /** Use only <code>unitValueInSIUnit</code> property for evaluation (not use <code>name</code> property),
    * so <code>x.compare(y) == 0</code> is not followed by <code>x.equals(y) == true<code>. */
  override def compare(that: U): Int = this.intervalInSIUnit.compare(that.intervalInSIUnit)
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
  override protected def newSymbolString: String = s"${firstUnit.symbol}*${secondUnit.symbol}"

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
  override protected def newSymbolString: String =
    if(denominatorUnit.isInstanceOf[LiteralComposite])
      s"${numeratorUnit.symbol}/(${denominatorUnit.symbol})"
    else
      s"${numeratorUnit.symbol}/${denominatorUnit.symbol}"

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