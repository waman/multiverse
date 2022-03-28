package org.waman.multiverse

import spire.implicits._
import spire.math.Real
import org.waman.multiverse.typeless.TypelessLinearUnit

trait PhysicalUnit[U <: PhysicalUnit[U]] { this: U =>
  def name: String

  def symbol: String
  def aliases: Seq[String]
  /** Return <code>this.symbol +: this.aliases </code>*/
  final def symbols: Seq[String] = symbol +: aliases

  def getSIUnit: U
  def isSIUnit: Boolean = getSIUnit == this

  /** Return the unit dimension in SI unit */
  def dimension: Map[DimensionSymbol, Int]

  /** Equivalent to <code>isEquivalentTo()</code> method. */
  final def ~=(that: PhysicalUnit[_]): Boolean = this.isEquivalentTo(that)

  def isEquivalentTo(that: PhysicalUnit[_]): Boolean

  def isTheSameUnitTypeAs(other: PhysicalUnit[_]): Boolean = 
    other match {
      case that: TypelessLinearUnit =>
        that.isTheSameUnitTypeAs(this)
      case that =>
        this.getSIUnit.eq(that.getSIUnit.asInstanceOf[AnyRef])
    }

  /** 
   * Return true if <code>this</code> is the same unit type as <code>that</code>
   * (the <code>isTheSameUnitTypeAs()</code> method returns true)
   * and has the same name as <code>that</code>.
   */
  override def equals(other: Any): Boolean = other match {
    case that: PhysicalUnit[_] => 
      that.canEqual(this) &&
        this.isTheSameUnitTypeAs(that) &&
        this.name == that.name
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PhysicalUnit[_]]

  override def hashCode: Int = (this.getSIUnit.getClass.getName, this.name).##
}

// Maybe only temperature
trait HomogeneousUnit[U <: HomogeneousUnit[U]] extends PhysicalUnit[U]{ this : U =>
  /** zero value in SI unit */
  def zero: Real
  /** interval in SI Unit */
  def interval: Real

  override def toString: String = {
    val ali: String = if (this.aliases.nonEmpty) this.aliases.mkString(", aliases: [", ", ", "]") else ""
    val dim: String = DimensionSymbol.toStringWithSymbol(this.dimension)
    val desc: String = this match {
      case d: Description =>  ", description: " + d.description
      case _ => ""
    }

    if(this == getSIUnit) {
      s"$name ($symbol)$ali, dim: $dim"
    }else{
      val symbolSI = getSIUnit.symbol
      val sZero = toReadableString(zero)
      this.interval match {
        case i if i.isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = Δ($symbolSI)]$ali, dim: $dim$desc"
        case i if (-i).isOne =>
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = -Δ($symbolSI)]$ali, dim: $dim$desc"
        case _ =>
          val sInterval = toReadableString(interval)
          s"$name ($symbol) [0($symbol) = $sZero($symbolSI), Δ($symbol) = $sInterval*Δ($symbolSI)]$ali, dim: $dim$desc"
      }
    }
  }

  override def isEquivalentTo(other: PhysicalUnit[_]): Boolean = 
    other match {
      case that: HomogeneousUnit[_] =>
        this.isTheSameUnitTypeAs(that) &&
          this.zero == that.zero &&
          this.interval == that.interval
      case _ => false
    }
}

trait Description {
  def description: String
}

trait NotExact