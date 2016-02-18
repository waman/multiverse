package org.waman.multiverse

import spire.math.Fractional

abstract class Quantity[A: Fractional, U <: PhysicalUnit[U]]
    extends Ordered[Quantity[A, U]]{

  val value: A
  val unit: U
  def apply(unit: U): A

  override def toString = s"$value (${unit.symbol})"

  override def equals(other: Any): Boolean = other match {
    case that: Quantity[A, U] =>
      if(!(that canEqual this))
        false
      else if(this.unit == that.unit)
        this.value == that.value
      else {
        val evalUnit = getSmallerUnit(that)
        this(evalUnit) == that(evalUnit)
      }

    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Quantity[_, _]]


  override def hashCode: Int =
    41 * (
      41 + value.hashCode
      ) + unit.hashCode

  override def compare(that: Quantity[A, U]): Int = {
    val evalUnit = getSmallerUnit(that)
    implicitly[Fractional[A]].compare(this(evalUnit), that(evalUnit))
  }

  private def getSmallerUnit(that: Quantity[A, U]): U =
    List(this, that).map(_.unit).min
}