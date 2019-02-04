package org.waman.multiverse

import spire.math.Fractional
import spire.implicits._

abstract class Quantity[A: Fractional, U <: PhysicalUnit[U]]
    extends Ordered[Quantity[A, U]]{

  val value: A
  val unit: U
  def apply(unit: U): A

//  override def toString = s"$value (${unit.symbols.mkString("|")})"

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

  override def hashCode: Int =
    41 * (
      41 + value.hashCode
      ) + unit.hashCode

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