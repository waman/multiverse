package org.waman.multiverse

trait PhysicalUnit{
  val name: String
  val symbol: String

  override def toString: String = s"$name ($symbol)"
}

trait ProductUnit[A <: PhysicalUnit, B <: PhysicalUnit] extends PhysicalUnit{
  def firstUnit: A
  def secondUnit: B

  override val name: String = s"${firstUnit.name}${secondUnit.name}"
  override val symbol: String = s"${firstUnit.symbol}${secondUnit.symbol}"

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

  override val name: String = s"${numeratorUnit.name}Per${denominatorUnit.name}"
  override val symbol: String = s"${numeratorUnit.symbol}${denominatorUnit.symbol}"

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