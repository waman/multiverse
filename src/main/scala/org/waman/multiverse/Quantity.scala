package org.waman.multiverse

import spire.math.{Fractional, Real}
import spire.implicits._

abstract class Quantity[A: Fractional, U <: PhysicalUnit[U]]
    extends Ordered[Quantity[A, U]] {

  def value: A
  def unit: U

  def dimension: Map[DimensionSymbol, Int] = unit.dimension

  def apply(evalUnit: U): A =
    if(unit == evalUnit) value
    else applyInDifferentUnit(evalUnit)

  protected def applyInDifferentUnit(evalUnit: U): A

  def getSIValue: A = this(unit.getSIUnit)

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
    (this(siUnit), siUnit).##
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

abstract class HomogeneousQuantity[A: Fractional, U <: HomogeneousUnit[U]] extends Quantity[A, U]{

  override protected def applyInDifferentUnit(evalUnit: U): A = {
    val algebra = implicitly[Fractional[A]]
    def real(r: Real): A = algebra.fromReal(r)
    (value * real(unit.interval) + real(unit.zero) - real(evalUnit.zero)) / real(evalUnit.interval)
  }
}

// maybe all quantities other than temperature
abstract class LinearQuantity[Q <: LinearQuantity[Q, A, U], A: Fractional, U <: LinearUnit[U]]
  extends Quantity[A, U] {

  protected def newQuantity(value: A, unit: U): Q

  override protected def applyInDifferentUnit(evalUnit: U): A = {
    val algebra: Fractional[A] = implicitly[Fractional[A]]
    value * algebra.fromReal(unit.interval) / algebra.fromReal(evalUnit.interval)
  }

  override protected def compareInDifferentUnit(that: Quantity[A, U]): Int = {
    val evalUnit = this.unit max that.unit
    implicitly[Fractional[A]].compare(this(evalUnit), that(evalUnit))
  }

  def +(that: Q): Q = {
    val u = this.unit max that.unit
    newQuantity(this(u) + that(u), u)
  }

  /** harmonic sum: v1v2/(v1 + v2) or v of 1/v =  1/v1 + 1/v2 */
  def |+|(that: Q): Q = {
    val u = this.unit max that.unit
    val v1 = this(u)
    val v2 = that(u)
    newQuantity(v1*v2/(v1 + v2), u)
  }

  def -(that: Q): Q = {
    val u = this.unit max that.unit
    newQuantity(this(u) - that(u), u)
  }

  def *(c: A): Q = newQuantity(this.value * c, this.unit)
  def /(c: A): Q = newQuantity(this.value / c, this.unit)
}