package org.waman.multiverse

import spire.math.Real

trait PhysicalUnit[U <: PhysicalUnit[U]] extends Ordered[U]{

  lazy val name: String = {
    val cName = getClass.getSimpleName
    cName.substring(0, cName.length - 1)  // drop the end char "$"
  }

  val symbols: Seq[String]

  def baseUnit: U
  def valueInBaseUnit: Real

  protected lazy val symbolStr = symbols.mkString(";")

  override def toString: String = s"$name ($symbolStr)"

  def toDetailString: String = {
    val s = s"${name.padTo(30, ' ')} ($symbolStr)"

    if (valueInBaseUnit == Real.one) s
    else {
      val eqSymbol = if(this.isInstanceOf[NotExact]) "~" else "="
      s.padTo(50, ' ') + s": 1 ${symbols.head.padTo(10, ' ')} $eqSymbol $valueInBaseUnit ${baseUnit.symbols.head}"
    }
  }

  override def compare(that: U): Int = this.valueInBaseUnit.compare(that.valueInBaseUnit)
}

trait NotExact

trait ProductUnit[U <: PhysicalUnit[U], A <: PhysicalUnit[A], B <: PhysicalUnit[B]]
    extends PhysicalUnit[U]{

  def firstUnit: A
  def secondUnit: B

  override lazy val name: String = s"${firstUnit.name}${secondUnit.name}"
  override lazy val symbols: Seq[String] =
    for(x <- firstUnit.symbols; y <- secondUnit.symbols)yield s"$x*$y"

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
  override lazy val symbols: Seq[String] =
    for(x <- numeratorUnit.symbols; y <- denominatorUnit.symbols) yield s"$x/$y"

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