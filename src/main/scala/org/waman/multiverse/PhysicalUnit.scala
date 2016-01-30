package org.waman.multiverse

import spire.math.Real

trait PhysicalUnit{
  lazy val name: String = {
    val cName = getClass.getSimpleName
    cName.substring(0, cName.length - 1)  // drop the end char "$"
  }
  val symbol: String

  protected def baseUnit: PhysicalUnit
  protected def inBaseUnitAccessor: () => Real

  override def toString: String = {
    val s = s"${name.padTo(20, ' ')} ($symbol)"
    val base = baseUnit

    if (this == base) s
    else s.padTo(30, ' ') + s": 1 ${symbol.padTo(5, ' ')} = ${inBaseUnitAccessor()} ${base.symbol}"
  }
}

trait ProductUnit[A <: PhysicalUnit, B <: PhysicalUnit] extends PhysicalUnit{
  def firstUnit: A
  def secondUnit: B

  override lazy val name: String = s"${firstUnit.name}${secondUnit.name}"
  override lazy val symbol: String = s"${firstUnit.symbol}${secondUnit.symbol}"

  override def equals(other: Any): Boolean = other match {
    case that: ProductUnit[_, _] =>
      (that canEqual this) &&
      firstUnit == that.firstUnit &&
      secondUnit == that.secondUnit
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ProductUnit[_, _]]

  override def hashCode: Int =
    41 * (
        41 + firstUnit.hashCode
      ) + secondUnit.hashCode
}

trait QuotientUnit[A <: PhysicalUnit, B <: PhysicalUnit] extends PhysicalUnit{
  def numeratorUnit: A
  def denominatorUnit: B

  override lazy val name: String = s"${numeratorUnit.name}Per${denominatorUnit.name}"
  override lazy val symbol: String = s"${numeratorUnit.symbol}${denominatorUnit.symbol}"

  override def equals(other: Any): Boolean = other match {
    case that: QuotientUnit[_, _] =>
      (that canEqual this) &&
        numeratorUnit == that.numeratorUnit &&
        denominatorUnit == that.denominatorUnit
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[QuotientUnit[_, _]]

  override def hashCode: Int =
    41 * (
      41 + numeratorUnit.hashCode
      ) + denominatorUnit.hashCode
}