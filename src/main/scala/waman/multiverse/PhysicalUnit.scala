package waman.multiverse

import spire.implicits._
import spire.math.Real

trait PhysicalUnit[U <: PhysicalUnit[U]] { this: U =>
  def name: String

  def symbol: String
  def aliases: Seq[String]
  /** Return <code>this.symbol +: this.aliases </code>*/
  final def symbols: Seq[String] = symbol +: aliases

  def getSIUnit: U

  /** Returen the unit dimension in SI unit */
  def dimension: Map[DimensionSymbol, Int]
}

// Maybe only temperature
trait HomogeneousUnit[U <: HomogeneousUnit[U]] extends PhysicalUnit[U]{ this : U =>
  /** zero value in SI unit */
  def zero: Real
  /** interval in SI Unit */
  def interval: Real

  /** Evaluate via <code>name</code>, <code>zero</zero>, <code>interval</code> and <code>dimension</code> properties
    * (not <code>symbol</code>). */
  override def equals(other: Any): Boolean = other match {
    case that: HomogeneousUnit[_] =>
      (that canEqual this) &&
        name == that.name &&
        dimension == that.dimension &&
        zero == that.zero &&
        interval == that.interval
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[PhysicalUnit[_]]

  override def hashCode: Int = (name, zero, interval, dimension).##

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

  def isEquivalentTo(other: HomogeneousUnit[_]): Boolean =
    this.dimension == other.dimension &&
      this.zero == other.zero &&
      this.interval == other.interval
}

trait Description {
  def description: String
}

trait NotExact