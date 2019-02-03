package org.waman.multiverse

import spire.math.Real

trait PhysicalUnit[U <: PhysicalUnit[U]] extends Ordered[U]{

//  val name: String
//  val symbols: Seq[String]

//  def getSIUnit: U
  def unitValueInSIUnit: Real

//  protected lazy val symbolStr: String = symbols.mkString(";")
//
//  override def toString: String = s"$name ($symbolStr)"
//
//  def toDetailString: String = {
//    val s = s"${name.padTo(30, ' ')} ($symbolStr)"
//
//    if (unitValueInSIUnit == Real.one) s
//    else {
//      val eqSymbol = if(this.isInstanceOf[NotExact]) "~" else "="
//      s.padTo(50, ' ') + s": 1 ${symbols.head.padTo(10, ' ')} $eqSymbol $unitValueInSIUnit ${getSIUnit.symbols.head}"
//    }
//  }

  override def compare(that: U): Int = this.unitValueInSIUnit.compare(that.unitValueInSIUnit)
}

object PhysicalUnit{
  def getBigger[U <: PhysicalUnit[U]](u:U, v: U): U = if(u.compare(v) >= 0) u else v
}

trait NotExact

//trait ProductUnit[U <: PhysicalUnit[U], A <: PhysicalUnit[A], B <: PhysicalUnit[B]]
//    extends PhysicalUnit[U]{
//
//  def firstUnit: A
//  def secondUnit: B
//
//  override lazy val name: String = s"${firstUnit.name}${secondUnit.name}"
//  override lazy val symbols: Seq[String] =
//    for(x <- firstUnit.symbols; y <- secondUnit.symbols)yield s"$x*$y"
//
//  override def equals(other: Any): Boolean = other match {
//    case that: ProductUnit[_, _, _] =>
//      (that canEqual this) &&
//      firstUnit == that.firstUnit &&
//      secondUnit == that.secondUnit
//    case _ => false
//  }
//
//  def canEqual(other: Any): Boolean = other.isInstanceOf[ProductUnit[_, _, _]]
//
//  override def hashCode: Int =
//    41 * (
//        41 + firstUnit.hashCode
//      ) + secondUnit.hashCode
//}
//
//trait QuotientUnit[U <: PhysicalUnit[U], A <: PhysicalUnit[A], B <: PhysicalUnit[B]]
//  extends PhysicalUnit[U]{
//
//  def numeratorUnit: A
//  def denominatorUnit: B
//
//  override lazy val name: String = s"${numeratorUnit.name}Per${denominatorUnit.name}"
//  override lazy val symbols: Seq[String] =
//    for(x <- numeratorUnit.symbols; y <- denominatorUnit.symbols) yield s"$x/$y"
//
//  override def equals(other: Any): Boolean = other match {
//    case that: QuotientUnit[_, _, _] =>
//      (that canEqual this) &&
//        numeratorUnit == that.numeratorUnit &&
//        denominatorUnit == that.denominatorUnit
//    case _ => false
//  }
//
//  def canEqual(other: Any): Boolean = other.isInstanceOf[QuotientUnit[_, _, _]]
//
//  override def hashCode: Int =
//    41 * (
//      41 + numeratorUnit.hashCode
//      ) + denominatorUnit.hashCode
//}