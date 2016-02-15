package org.waman.multiverse

import spire.math.Real

trait PhysicalUnit[U <: PhysicalUnit[U]]{

  lazy val name: String = {
    val cName = getClass.getSimpleName
    cName.substring(0, cName.length - 1)  // drop the end char "$"
  }
  val symbol: String

  def baseUnit: U
  def inBaseUnitAccessor: () => Real

  override def toString: String = s"$name ($symbol)"

  def toDetailString: String = {
    val s = s"${name.padTo(30, ' ')} ($symbol)"
    val valueInBaseUnit = inBaseUnitAccessor()

    if (valueInBaseUnit == Real.one) s
    else {
      val eqSymbol = if(this.isInstanceOf[NotExact]) "~" else "="
      s.padTo(45, ' ') + s": 1 ${symbol.padTo(8, ' ')} $eqSymbol $valueInBaseUnit ${baseUnit.symbol}"
    }
  }
}

trait NotExact

trait ProductUnit[U <: PhysicalUnit[U], A <: PhysicalUnit[A], B <: PhysicalUnit[B]]
    extends PhysicalUnit[U]{

  def firstUnit: A
  def secondUnit: B

  override lazy val name: String = s"${firstUnit.name}${secondUnit.name}"
  override lazy val symbol: String = s"${firstUnit.symbol}${secondUnit.symbol}"

  override def equals(other: Any): Boolean = other match {
    case that: ProductUnit[_, _, _] =>
      (that canEqual this) &&
      firstUnit == that.firstUnit &&
      secondUnit == that.secondUnit
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ProductUnit[_, _, _]]

  override def hashCode: Int =
    41 * (
        41 + firstUnit.hashCode
      ) + secondUnit.hashCode
}

trait QuotientUnit[U <: PhysicalUnit[U], A <: PhysicalUnit[A], B <: PhysicalUnit[B]]
  extends PhysicalUnit[U]{

  def numeratorUnit: A
  def denominatorUnit: B

  override lazy val name: String = s"${numeratorUnit.name}Per${denominatorUnit.name}"
  override lazy val symbol: String = s"${numeratorUnit.symbol}/${denominatorUnit.symbol}"

  override def equals(other: Any): Boolean = other match {
    case that: QuotientUnit[_, _, _] =>
      (that canEqual this) &&
        numeratorUnit == that.numeratorUnit &&
        denominatorUnit == that.denominatorUnit
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[QuotientUnit[_, _, _]]

  override def hashCode: Int =
    41 * (
      41 + numeratorUnit.hashCode
      ) + denominatorUnit.hashCode
}