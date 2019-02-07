package org.waman.multiverse

import spire.math.Fractional
import spire.implicits._

abstract class Quantity[A: Fractional, U <: PhysicalUnit[U]]
    extends Ordered[Quantity[A, U]]{

  val value: A
  val unit: U
  def apply(unit: U): A

  override def equals(other: Any): Boolean = other match {
    case that: Quantity[A, U] =>
      if(!(that canEqual this))
        false
      else if(this.unit == that.unit)
        this.value == that.value
      else {
        val evalUnit = PhysicalUnit.getBigger(this.unit, that.unit)
        this(evalUnit) == that(evalUnit)
      }

    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Quantity[_, _]]

  override lazy val hashCode: Int = {
    val siUnit = unit.getSIUnit
    41 * (
      41 + this(siUnit).hashCode
      ) + siUnit.hashCode
  }

  override def toString: String = toString("(", ")")

  // for 1.0(m),
  // open ~ (
  // close ~ )
  def toString(open: String, close: String): String = s"$value$open${unit.symbol}$close"

  override def compare(that: Quantity[A, U]): Int = {
    val evalUnit = PhysicalUnit.getBigger(this.unit, that.unit)
    implicitly[Fractional[A]].compare(this(evalUnit), that(evalUnit))
  }
}

// maybe all quantities other than temperature
abstract class LinearQuantity[A: Fractional, U <: PhysicalUnit[U]]
  extends Quantity[A, U] {

  def apply(evalUnit: U): A =
    if(unit == evalUnit){
      value
    } else {
      val algebra: Fractional[A] = implicitly[Fractional[A]]
      value * algebra.fromReal(unit.unitValueInSIUnit) / algebra.fromReal(evalUnit.unitValueInSIUnit)
    }
}

abstract class ExtensiveQuantity[Q <: ExtensiveQuantity[Q, A, U], A: Fractional, U <: PhysicalUnit[U]]
  extends LinearQuantity[A, U]{

  protected def newQuantity(value: A, unit: U): Q

  def +(that: Q): Q = {
    val u = PhysicalUnit.getBigger(this.unit, that.unit)
    val value = this(u) + that(u)
    newQuantity(value, u)
  }

  def -(that: Q): Q = {
    val u = PhysicalUnit.getBigger(this.unit, that.unit)
    val value = this(u) - that(u)
    newQuantity(value, u)
  }

  def *(c: A): Q = newQuantity(this.value * c, this.unit)
  def /(c: A): Q = newQuantity(this.value / c, this.unit)
}