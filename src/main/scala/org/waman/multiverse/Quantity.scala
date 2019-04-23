package org.waman.multiverse

import spire.math.{Fractional, Real}
import spire.implicits._

abstract class Quantity[A: Fractional, U <: PhysicalUnit[U]]
    extends Ordered[Quantity[A, U]]{

  def value: A
  def unit: U

  def apply(evalUnit: U): A =
    if(unit == evalUnit) value
    else applyInDifferentUnit(evalUnit)

  protected def applyInDifferentUnit(evalUnit: U): A = {
    val algebra = implicitly[Fractional[A]]
    def real(r: Real): A = algebra.fromReal(r)
    (value * real(unit.intervalInSIUnit) + real(unit.zeroInSIUnit) - real(evalUnit.zeroInSIUnit)) / real(evalUnit.intervalInSIUnit)
  }

  override def equals(other: Any): Boolean = other match {
    case that: Quantity[A, U] =>
      if(!(that canEqual this))
        false
      else if(this.unit == that.unit)
        this.value == that.value
      else {
        equalsInDifferentUnit(that)
      }

    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Quantity[_, _]]

  protected def equalsInDifferentUnit(that: Quantity[A, U]): Boolean = {
    val siUnit = unit.getSIUnit
    this(siUnit) == that(siUnit)
  }

  override lazy val hashCode: Int = {
    val siUnit = unit.getSIUnit
    41 * (
      41 + this(siUnit).hashCode
      ) + siUnit.hashCode
  }

  override def toString: String = toString("(", ")")

  // $value$open$symbol$close
  // For 1.0(m),
  // open: (
  // close : )
  def toString(open: String, close: String): String = s"$value$open${unit.symbol}$close"

  override def compare(that: Quantity[A, U]): Int = {
    if(this.unit == that.unit)
      implicitly[Fractional[A]].compare(this.value, that.value)
    else
      compareInDifferentUnit(that)
  }

  protected def compareInDifferentUnit(that: Quantity[A, U]): Int = {
    val siUnit = unit.getSIUnit
    implicitly[Fractional[A]].compare(this(siUnit), that(siUnit))
  }
}

// maybe all quantities other than temperature
abstract class ScaleQuantity[A: Fractional, U <: ScaleUnit[U]]
  extends Quantity[A, U] {

  override protected def applyInDifferentUnit(evalUnit: U): A = {
    val algebra: Fractional[A] = implicitly[Fractional[A]]
    value * algebra.fromReal(unit.intervalInSIUnit) / algebra.fromReal(evalUnit.intervalInSIUnit)
  }

  override protected def equalsInDifferentUnit(that: Quantity[A, U]): Boolean = {
    val evalUnit = this.unit max that.unit
    this(evalUnit) == that(evalUnit)
  }

  override def compareInDifferentUnit(that: Quantity[A, U]): Int = {
    val evalUnit = this.unit max that.unit
    implicitly[Fractional[A]].compare(this(evalUnit), that(evalUnit))
  }
}

abstract class ExtensiveQuantity[Q <: ExtensiveQuantity[Q, A, U], A: Fractional, U <: ScaleUnit[U]]
  extends ScaleQuantity[A, U]{

  protected def newQuantity(value: A, unit: U): Q

  def +(that: Q): Q = {
    val u = this.unit max that.unit
    val value = this(u) + that(u)
    newQuantity(value, u)
  }

  def -(that: Q): Q = {
    val u = this.unit max that.unit
    val value = this(u) - that(u)
    newQuantity(value, u)
  }

  def *(c: A): Q = newQuantity(this.value * c, this.unit)
  def /(c: A): Q = newQuantity(this.value / c, this.unit)
}