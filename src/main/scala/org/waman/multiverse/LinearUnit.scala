package org.waman.multiverse

import spire.math.Real

trait LinearUnit[U <: LinearUnit[U]] extends HomogeneousUnit[U] with Ordered[U]{ this: U =>

  override def zero: Real = 0

  /** Use <code>name</code> and <code>interval</code> properties (not <code>symbol</code>) for equality evaluation. */
  override def equals(other: Any): Boolean = other match {
    case that: LinearUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        interval == that.interval
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[LinearUnit[_]]

  override def hashCode: Int =
    41 * (
      41 + name.hashCode
      ) + interval.hashCode

  override def toString: String = {
    val sInterval = toReadableString(interval)
    this match {
      case thisUnit if thisUnit == getSIUnit =>
        s"$name ($symbol)"
      case _: NotExact =>
        s"$name ($symbol) [1($symbol) ≈ $sInterval(${getSIUnit.symbol})]"
      case _ =>
        s"$name ($symbol) [1($symbol) = $sInterval(${getSIUnit.symbol})]"
    }
  }

  /** Use only <code>interval</code> property for evaluation (not use <code>name</code> property),
    * so <code>x.compare(y) == 0</code> is not followed by <code>x.equals(y) == true<code>. */
  override def compare(that: U): Int = this.interval.compare(that.interval)

  def *[V <: LinearUnit[V]](secondUnit: V): PUnit[U, V] =
    new PUnit[U, V](LinearUnit.this, secondUnit)

  def /[V <: LinearUnit[V]](denominatorUnit: V): QUnit[U, V] =
    new QUnit[U, V](LinearUnit.this, denominatorUnit)

  def max(that: U): U = if((this compare that) >= 0) this else that
  def min(that: U): U = if((this compare that) <= 0) this else that
}

// For symbol property of QuotientUnit: m/(s*ms)
// Tests are written in AccelerationSpec
private[multiverse] trait LiteralComposite

abstract class ProductUnit[U <: LinearUnit[U], A <: LinearUnit[A], B <: LinearUnit[B]]
  (val firstUnit: A, val secondUnit: B)
  extends LinearUnit[U] with LiteralComposite { this: U =>

  override val name: String = s"${firstUnit.name} times ${secondUnit.name}"
  override val symbol: String = s"${firstUnit.symbol}*${secondUnit.symbol}"
  override def aliases: Seq[String] =
    secondUnit.symbols.flatMap(s => firstUnit.symbols.map(f => s"$f*$s")).tail

  override val interval: Real = firstUnit.interval * secondUnit.interval

  override def dimension: Map[DimensionSymbol, Int] =
    DimensionSymbol.values.map(s => (s, firstUnit.dimension(s) + secondUnit.dimension(s))).toMap

  override def equals(other: Any): Boolean = other match {
    case that: ProductUnit[_, _, _] =>
      (that canEqual this) &&
        firstUnit == that.firstUnit &&
        secondUnit == that.secondUnit
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[ProductUnit[_, _, _]]

  override def hashCode: Int =
    41 * (
      41 * (
        41 + firstUnit.hashCode
        ) + "*".hashCode
      ) + secondUnit.hashCode
}

class PUnit[A <: LinearUnit[A], B <: LinearUnit[B]](firstUnit: A, secondUnit: B)
  extends ProductUnit[PUnit[A, B], A, B](firstUnit, secondUnit){ this: PUnit[A, B] =>

  override def getSIUnit: PUnit[A, B] = this.firstUnit.getSIUnit * this.secondUnit.getSIUnit
  override lazy val dimension: Map[DimensionSymbol, Int] = super.dimension.filter(_._2 != 0).withDefaultValue(0)
}

abstract class QuotientUnit[U <: LinearUnit[U], A <: LinearUnit[A], B <: LinearUnit[B]]
  (val numeratorUnit: A, val denominatorUnit: B)
  extends LinearUnit[U] with LiteralComposite { this: U =>

  override val name: String = s"${numeratorUnit.name} per ${denominatorUnit.name}"
  override val symbol: String =
    if(denominatorUnit.isInstanceOf[LiteralComposite])
      s"${numeratorUnit.symbol}/(${denominatorUnit.symbol})"
    else
      s"${numeratorUnit.symbol}/${denominatorUnit.symbol}"

  override def aliases: Seq[String] =
    denominatorUnit.symbols.flatMap(d => numeratorUnit.symbols.map(n => s"$n/$d")).tail

  override val interval: Real = numeratorUnit.interval / denominatorUnit.interval

  override def dimension: Map[DimensionSymbol, Int] =
    DimensionSymbol.values.map(s => (s, numeratorUnit.dimension(s) - denominatorUnit.dimension(s))).toMap

  override def equals(other: Any): Boolean = other match {
    case that: QuotientUnit[_, _, _] =>
      (that canEqual this) &&
        numeratorUnit == that.numeratorUnit &&
        denominatorUnit == that.denominatorUnit
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[QuotientUnit[_, _, _]]

  override def hashCode: Int =
    41 * (
      41 * (
        41 + numeratorUnit.hashCode
        ) + "/".hashCode
      ) + denominatorUnit.hashCode
}

class QUnit[A <: LinearUnit[A], B <: LinearUnit[B]](numeratorUnit: A, denominatorUnit: B)
  extends QuotientUnit[QUnit[A, B], A, B](numeratorUnit, denominatorUnit){ this: QUnit[A, B] =>

  override def getSIUnit: QUnit[A, B] = this.numeratorUnit.getSIUnit / this.denominatorUnit.getSIUnit
  override lazy val dimension: Map[DimensionSymbol, Int] = super.dimension.filter(_._2 != 0).withDefaultValue(0)
}