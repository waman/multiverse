package org.waman.multiverse

import spire.math.Real

trait LinearUnit[U <: LinearUnit[U]] extends PhysicalUnit[U] with Ordered[U]{ this: U =>

  override def zeroInSIUnit: Real = 0

  /** Use <code>name</code> and <code>unitValueInSIUnit</code> properties (not <code>symbol</code>) for equality evaluation. */
  override def equals(other: Any): Boolean = other match {
    case that: LinearUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        intervalInSIUnit == that.intervalInSIUnit
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[LinearUnit[_]]

  override def hashCode: Int =
    41 * (
      41 + name.hashCode
      ) + intervalInSIUnit.hashCode

  override def toString: String = {
    val sInterval = toReadableString(intervalInSIUnit)
    this match {
      case thisUnit if thisUnit == getSIUnit =>
        s"$name ($symbol)"
      case _: NotExact =>
        s"$name ($symbol) [1($symbol) ≈ $sInterval(${getSIUnit.symbol})]"
      case _ =>
        s"$name ($symbol) [1($symbol) = $sInterval(${getSIUnit.symbol})]"
    }
  }

  /** Use only <code>unitValueInSIUnit</code> property for evaluation (not use <code>name</code> property),
    * so <code>x.compare(y) == 0</code> is not followed by <code>x.equals(y) == true<code>. */
  override def compare(that: U): Int = this.intervalInSIUnit.compare(that.intervalInSIUnit)

  def max(that: U): U = if((this compare that) >= 0) this else that
  def min(that: U): U = if((this compare that) <= 0) this else that
}