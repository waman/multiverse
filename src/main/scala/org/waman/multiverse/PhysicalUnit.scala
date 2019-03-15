package org.waman.multiverse

import spire.math.Real
import scala.reflect.runtime.{universe => ru}

trait PhysicalUnit[U <: PhysicalUnit[U]] extends Ordered[U]{

  def name: String
  lazy val symbol: String = extractSymbol

  protected def extractSymbol: String =  {
    val im = ru.runtimeMirror(getClass.getClassLoader).reflect(this)
    im.symbol.name.toString
  }

  def getSIUnit: U
  def unitValueInSIUnit: Real

  /** Use <code>name</code> and <code>unitValueInSIUnit</code> properties (not <code>symbol</code>) for equality evaluation. */
  override def equals(other: Any): Boolean = other match {
    case that: PhysicalUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        unitValueInSIUnit == that.unitValueInSIUnit
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PhysicalUnit[_]]

  override def hashCode: Int =
    41 * (
      41 + name.hashCode
      ) + unitValueInSIUnit.hashCode

  override def toString: String = getSIUnit match {
    case u if this == u =>
      s"$name ($symbol)"
    case _ =>
      s"$name ($symbol) [1($symbol) = $unitValueInSIUnit(${getSIUnit.symbol})]"
  }

  /** Use only <code>unitValueInSIUnit</code> property for evaluation (not use <code>name</code> property),
    * so <code>x.compare(y) == 0</code> is not followed by <code>x.equals(y) == true<code>. */
  override def compare(that: U): Int = this.unitValueInSIUnit.compare(that.unitValueInSIUnit)
}

object PhysicalUnit{
  def getBigger[U <: PhysicalUnit[U]](u:U, v: U): U = if(u.compare(v) >= 0) u else v
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
  override protected def extractSymbol: String = s"${firstUnit.symbol}*${secondUnit.symbol}"

  override val unitValueInSIUnit: Real = firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit

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
  override protected def extractSymbol: String =
    if(denominatorUnit.isInstanceOf[LiteralComposite])
      s"${numeratorUnit.symbol}/(${denominatorUnit.symbol})"
    else
      s"${numeratorUnit.symbol}/${denominatorUnit.symbol}"

  override val unitValueInSIUnit: Real = numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit

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