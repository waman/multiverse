package org.waman.multiverse

import spire.math.Real
import org.waman.multiverse.typeless._
import org.waman.multiverse.typeless.TypelessLinearUnit

trait LinearUnit[U <: LinearUnit[U]] extends HomogeneousUnit[U] with Ordered[U]{ this: U =>

  override final def zero: Real = Real.zero

  /** Use only <code>interval</code> property for evaluation (not use <code>name</code> property),
    * so <code>x.compare(y) == 0</code> is not followed by <code>x.equals(y) == true<code>. */
  override def compare(that: U): Int = this.interval.compare(that.interval)

  /**
    * This multiplication returns a typeless unit.
    */
  def *[V <: LinearUnit[V]](secondUnit: V): TypelessLinearUnit =
    this.asTypeless.multiply(secondUnit.asTypeless)

  def ^(n: Int): TypelessLinearUnit = this.asTypeless^n

  /** Equivalent to ^ operator. */
  final def **(n: Int): TypelessLinearUnit = this^n

  /**
    * This division returns a typeless unit.
    */
  def /[V <: LinearUnit[V]](denominatorUnit: V): TypelessLinearUnit =
    this.asTypeless.divide(denominatorUnit.asTypeless)

  def reciprocal: TypelessLinearUnit = this.asTypeless.reciprocal

  def asTypeless: TypelessLinearUnit = new SimpleTypelessLinearUnit(this)

  def max(that: U): U = if((this compare that) >= 0) this else that
  def min(that: U): U = if((this compare that) <= 0) this else that

  /**
    * Return true if <code>this.dimension == other.dimension && this.interval == other.interval</code>,
    *  otherwise false. (Note: <code>name</code> property is not used for evaluation not like <code>equals</code> method.)
    */
  def isEquivalentTo(other: LinearUnit[_]): Boolean =
    this.dimension == other.dimension && this.interval == other.interval

  /** Evaluate via <code>name</code>, <code>interval</code> and <code>dimension</code> properties
    * (not <code>symbol</code>). */
  override def equals(other: Any): Boolean = other match {
    case that: LinearUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        interval == that.interval &&
        dimension == that.dimension
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[LinearUnit[_]]

  override def hashCode: Int = (name, interval, dimension).##

  override def toString: String = {
    val sInterval = toReadableString(interval)
    val ali: String = if (this.aliases.nonEmpty) this.aliases.mkString(", aliases: [", ", ", "]") else ""
    val dim: String = DimensionSymbol.toStringWithSymbol(this.dimension)
    val desc: String = this match {
      case d: Description => ", description: " + d.description
      case _ => ""
    }

    this match {
      case _ if this == getSIUnit =>
        s"$name ($symbol)$ali, dim: $dim$desc"
      case _: NotExact =>
        s"$name ($symbol) [1($symbol) ≈ $sInterval(${getSIUnit.symbol})]$ali, dim: $dim$desc"
      case _ =>
        s"$name ($symbol) [1($symbol) = $sInterval(${getSIUnit.symbol})]$ali, dim: $dim$desc"
    }
  }
}

// For symbol property of QuotientUnit: m/(s*ms)
trait LiteralComposite

object LiteralComposite {
  def mkSymbol(u: LinearUnit[_]): String = u match {
    case _: LiteralComposite => s"(${u.symbol})"
    case _ => u.symbol
  }
}

abstract class ProductUnit[U <: LinearUnit[U], A <: LinearUnit[A], B <: LinearUnit[B]](val firstUnit: A, val secondUnit: B)
  extends LinearUnit[U] with LiteralComposite { this: U =>

  override val name: String = s"${firstUnit.name} times ${secondUnit.name}"

  override val symbol: String = this.firstUnit match {
    case _: QuotientUnit[_, _, _] =>
      s"(${this.firstUnit.symbol})*${this.secondUnit.symbol}"
    case _ =>
      s"${this.firstUnit.symbol}*${this.secondUnit.symbol}"
  }
  override val interval: Real = firstUnit.interval * secondUnit.interval

  override def aliases: Seq[String] =
    secondUnit.symbols.flatMap(s => firstUnit.symbols.map(f => s"$f*$s")).tail

  override def dimension: Map[DimensionSymbol, Int] =
    DimensionSymbol.values.map(s => (s, firstUnit.dimension(s) + secondUnit.dimension(s))).toMap

  override def asTypeless: TypelessLinearUnit =
     this.firstUnit.asTypeless * this.secondUnit.asTypeless
}

abstract class QuotientUnit[U <: LinearUnit[U], A <: LinearUnit[A], B <: LinearUnit[B]]
  (val numeratorUnit: A, val denominatorUnit: B)
  extends LinearUnit[U] with LiteralComposite { this: U =>

  override val name: String = s"${numeratorUnit.name} per ${denominatorUnit.name}"

  override val symbol: String = this.denominatorUnit match {
    case _: LiteralComposite =>
      s"${this.numeratorUnit.symbol}/(${this.denominatorUnit.symbol})"
    case _ =>
      s"${this.numeratorUnit.symbol}/${this.denominatorUnit.symbol}"
  }

  override def aliases: Seq[String] = {
    def aliasesStrings(f: String => String): Seq[String] =
      denominatorUnit.symbols.flatMap(d => numeratorUnit.symbols.map(n => s"$n/${f(d)}")).tail

    this.denominatorUnit match {
      case _: LiteralComposite =>
        aliasesStrings(s => s"($s)")
      case _ =>
        aliasesStrings(s => s)
    }
  }

  override val interval: Real = numeratorUnit.interval / denominatorUnit.interval

  override def dimension: Map[DimensionSymbol, Int] =
    DimensionSymbol.values.map(s => (s, numeratorUnit.dimension(s) - denominatorUnit.dimension(s))).toMap

  override def asTypeless: TypelessLinearUnit =
    this.numeratorUnit.asTypeless / this.denominatorUnit.asTypeless
}